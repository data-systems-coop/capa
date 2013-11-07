{-# Language OverloadedStrings, ScopedTypeVariables #-}
module Main (main) where

import Types
import Utils
import Domain
import Serialize
import Persist.Persist
import Service.Service
import View
import WebServer

import Happstack.Lite(serveDirectory, Browsing(..))
import Happstack.Lite(method, dir, path, Method(..), nullDir, msum)
import Happstack.Server(dirs, decodeBody, defaultBodyPolicy)
import Happstack.Lite(ServerPart)
import Control.Monad.Reader

import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as EX --util

import Data.Acid(openLocalState)
import qualified Data.Map as M
import qualified Database.HDBC.PostgreSQL as PG -- remove me
import Database.HDBC(disconnect)

import Data.Either.Utils (forceEither)
import qualified Data.ConfigFile as CF

import qualified System.Log.Handler.Syslog as SYS
import System.Environment(getArgs, getProgName)
import System.Log.Logger(traplogging, Priority)

-- run db action, cleanup conn resources
withConn :: String -> (PG.Connection -> ServerPartR) -> ServerPartR
withConn connString body = do -- use bracket instead
  cn <- liftIO $ PG.connectPostgreSQL connString
  r <- body cn  
  liftIO $ disconnect cn
  return r

nGET :: ServerPart ()
nGET = nullDir >> method GET

nPOST :: ServerPart ()
nPOST = nullDir >> method POST
  
-- Main.hs
capaApp :: 
  PersistConnection -> String -> (String -> ServerPartR) -> 
  (String -> String -> PG.Connection -> ServerPartR) ->
  TemplateStore -> ServerPartR
capaApp ref connStr authControl resolveCoopWith hState =
 let loginUrl = "/control/enter"
     withCn = withConn connStr
     resolveCoopCtrl = withCn $ resolveCoopWith loginUrl "/control/coop/summary"
     templateResponseWithState = 
       generalTemplateResponse   
         loginUrl
         "/control/coop/register/partial?alloc=%s&disburse=%s" 
         (templateFor hState) ref
     unsecuredTemplateResponse nm = withCn $ templateResponseWithState False False nm
     templateResponse nm = withCn $ templateResponseWithState True True nm
     noPartialTemplateResponse nm = withCn $ templateResponseWithState True False nm
     w handler = do 
       cn <- liftIO $ PG.connectPostgreSQL connStr
       r <- runReaderT handler (ref, cn)
       liftIO $ disconnect cn
       return r
 in do 
 decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
 msum [
   -- Main.hs
    dir "img" $ serveDirectory EnableBrowsing [] "control/images"
  , dir "js" $ serveDirectory EnableBrowsing [] "control/js"
    --partial path failurs like missing parameter?
  --view router
  , dir "control" $ msum [ --remove control 
         dir "coop" $ msum [
            dir "summary" $ templateResponse "coopSummary"
          , dir "register" $ msum [
                unsecuredTemplateResponse "registerCoop"
              , dir "partial" $ noPartialTemplateResponse "partialRegistration"
              , dir "authenticate" $ unsecuredTemplateResponse "registerAuthenticate"]
          , dir "settings" $ msum [
              noPartialTemplateResponse "coopSettings"
            , dir "disburse" $ dir "schedule" $ 
                  noPartialTemplateResponse "setDefaultDisbursalSchedule"
            , dir "show" $ templateResponse "showCoopSettings"] ]
       , dir "member" $ msum [
            dir "account" $ dir "action" $ dir "add" $ 
              templateResponse "addAction"
          , dir "patronage" $ dir "record" $ 
              templateResponse "recordPatronage"]
       , dir "members" $ msum [
             dir "accounts" $ templateResponse "memberAccounts"
           , dir "patronage" $ dir "period" $ templateResponse "periodPatronage"
           , dir "add" $ templateResponse "newMember"]
       , dir "financial" $ dir "results" $ msum [
             templateResponse "financialResults"
           , dir "record" $ templateResponse "recordResult"]
       , dirs "equity" $ msum [ 
            dir "members"  $ dir "allocationsDisbursals" $ 
              templateResponse "allocationsDisbursals"] 
       , dir "enter" $ unsecuredTemplateResponse "enter" 
       , dir "login" $ msum [
             dir "resolve" $ dir "coop" $ method POST >> resolveCoopCtrl
           , dir "register" $ authControl "/control/coop/register?username="]
       , dir "logout" $ 
           runReaderT expireSession ref >> redirect loginUrl
       , dir "export" $ templateResponse "export"
       , redirect loginUrl]
  
  --service router
  , dir "financial" $ dir "results" $ msum [   -- change to api/  
       nGET >> w getAllFinancialResultsDetail, 
       nPOST >> w putFinancialResults, 
       dir "delete" $ 
         nPOST >> w deleteFinancialResults]
  , dir "allocation" $ msum [
         nGET >> w getAllocation
       , dir "disburse" $ dir "actions" $ 
           nGET >> w getAllocationDisbursals]
  , dir "members" $ msum [
        nGET >> w getMembers
      , dir "equity" $ msum [
          dir "accounts" $ 
            nGET >> w getAllMembersEquityAccounts]
      , dir "patronage" $ 
          path $ \(fiscalPeriodStr::String) -> 
              nGET >> w (getAllMemberPatronage fiscalPeriodStr)]
  , dir "member" $ msum [ 
         nPOST >> w putMemberAndAccounts
       , path $ \(mid::Integer) -> 
           nGET >> w (getMember mid)
       , msum [ 
           path $ \(idIn::Integer) -> 
             dir "patronage" $ 
               path $ \(performedOverStr::String) -> msum [
                   nPOST >> w (putMemberPatronage idIn performedOverStr)
                 , dir "delete" $ 
                     nPOST >> w (deleteMemberPatronage idIn performedOverStr)]]
       , dir "equity" $ msum [
           dir "disburse" $ 
             nPOST >> w postScheduleAllocateDisbursal
         , dir "history" $ 
             nPOST >> w putEquityAction
         , dir "account" $ msum [
                nGET >> w getMemberEquityAccount
              , dir "actions" $ 
                  nGET >> w getActionsForMemberEquityAcct] ] ]
  , dir "equity" $ msum [
       dir "members" $ msum [
         dir "allocate" $ msum [
             dir "generate" $ 
               nPOST >> w postAllocateToMembers
           , dir "save" $ 
               nPOST >> w postAllocationDisbursal] ] ]
  , dir "coop" $ msum [
       nGET >> w getCooperative 
     , nPOST >> w putCooperative
     , dir "settings" $ msum [
          dir "allocate" $ msum [ 
               nPOST >> w putCoopAllocateSettings
             , dir "method" $ 
                 nGET >> w getAllocMethodDetail
             , dir "seniority" $ dir "levels" $ 
                 nGET >> w getSeniorityMappings] 
        , dir "disburse" $ dir "schedule" $ dir "default" $ msum [
               nPOST >> w putDefaultDisbursalSchedule
             , nGET >> w getDefaultDisbursalSchedule] ] ] 
  , dir "fiscal" $ dir "periods" $ 
      nGET >> w getLatestFiscalPeriods
  , dir "allocate" $ msum [
      dir "method" $ path $ 
        (okJSResp . fieldDetails . read) --GET
    , dir "methods" $ 
        nGET >> (okJSResp $ fmap show allocMethods)]
  , dir "export.zip" $ 
      nGET >> w exportAll
  , dir "logout" $ 
      runReaderT expireSession ref --POST
  , redirect loginUrl]

g0 = Globals M.empty

-- Main.hs
run :: IO ()  
run = do
  (configFile:_) <- getArgs
  prog <- getProgName
  cp <- fmap forceEither $ CF.readfile CF.emptyCP configFile
  setupLogging prog $ getConfig cp "rootloglevel"
  infoM "main" "started"
  let authUriBase = getConfig cp "authuribase"
  let servicesUri = getConfig cp "servicesuri"::String
  let connString = 
       printf "host=%s port=%d dbname=%s user=%s password=%s"
         (getConfig cp "dbhost"::String)
         (getConfig cp "dbport"::Integer)
         (getConfig cp "dbname"::String)
         (getConfig cp "dbuser"::String)
         (getConfig cp "dbpass"::String)
  let webPort = getConfig cp "webport"
  socket <- openSocket webPort $ getConfig cp "webprocessuser"  
  x <- openLocalState g0   --restart cache  
  ehs <- initTemplateRepo $ getConfig cp "templatedir"
  either 
    (error . concat) --output init template errors
    (serve socket webPort . 
     capaApp 
       x 
       connString
       (authenticatedForRegister authUriBase)
       (resolveCoop authUriBase x)) --provide auth handlers
    ehs 

getConfig :: (CF.Get_C a) => CF.ConfigParser -> CF.OptionSpec -> a
getConfig cp = forceEither . CF.get cp "DEFAULT"

setupLogging :: String -> Priority -> IO ()
setupLogging progName rootLogLevel = do 
  s <- SYS.openlog progName [] SYS.USER DEBUG  
  updateGlobalLogger rootLoggerName (addHandler s . setLevel rootLogLevel)

main :: IO ()
main = traplogging "main" ERROR "Server quit due to" run 
