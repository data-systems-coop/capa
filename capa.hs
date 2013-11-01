{-# Language OverloadedStrings #-}
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
       dir "delete" $ nPOST >> w deleteFinancialResults]
  , dir "allocation" $ msum [
         nGET >> w getAllocation
       , dir "disburse" $ dir "actions" $ nGET >> w getAllocationDisbursals]
  , dir "members" $ msum [
        nGET >> w getMembers
      , dir "equity" $ msum [
           dir "accounts" $ nGET >> w getAllMembersEquityAccounts]
      , dir "patronage" $ method GET >> w getAllMemberPatronage]
  , dir "member" $ msum [ 
         nPOST >> w putMemberAndAccounts, method GET >> w getMember
       , msum [ 
            method POST >> w putMemberPatronage
          , method POST >> w deleteMemberPatronage]
       , dir "equity" $ msum [
           dir "disburse" $ nPOST >> w postScheduleAllocateDisbursal
         , dir "history" $ nPOST >> w putEquityAction
         , dir "account" $ msum [
                nGET >> w getMemberEquityAccount
              , dir "actions" $ nGET >> w getActionsForMemberEquityAcct] ] ]
  , dir "equity" $ msum [
       dir "members" $ msum [
         dir "allocate" $ msum [
             dir "generate" $ nPOST >> w postAllocateToMembers
           , dir "save" $ nPOST >> w postAllocationDisbursal] ] ]
  , dir "coop" $ msum [
       nGET >> w getCooperative, nPOST >> w putCooperative
     , dir "settings" $ msum [
          dir "allocate" $ msum [ 
               nPOST >> w putCoopAllocateSettings
             , dir "method" $ nGET >> w getAllocMethodDetail
             , dir "seniority" $ dir "levels" $ nGET >> w getSeniorityMappings] 
        , dir "disburse" $ dir "schedule" $ dir "default" $ msum [
                 nPOST >> w putDefaultDisbursalSchedule
               , nGET >> w getDefaultDisbursalSchedule] ] ] 
  , dir "fiscal" $ dir "periods" $ w getLatestFiscalPeriods
  , dir "allocate" $ msum [
      dir "method" $ path $ (okJSResp . fieldDetails . read) --GET
    , dir "methods" $ nGET >> (okJSResp $ fmap show allocMethods)]
  , dir "export.zip" $ nGET >> w exportAll
  , dir "logout" $ runReaderT expireSession ref
  , redirect loginUrl]

g0 = Globals M.empty

-- Main.hs
run :: IO ()  
run = do
  --receive config file arg
  --setup logger
  s <- SYS.openlog "capa" [] SYS.USER DEBUG
  -- config should select debug or not    
  updateGlobalLogger rootLoggerName (addHandler s . setLevel DEBUG)
  infoM "main" "started"
  --read config
  val <- CF.readfile CF.emptyCP "etc/dev.txt"
  let cp = forceEither val
  let getConfig = forceEither . CF.get cp "DEFAULT" 
  --read authent config
  let authUriBase = (getConfig "authuribase")::String
  --read service config
  let servicesUri = (getConfig "servicesuri")::String
  --build db string
  let connString = 
       printf "host=%s port=%d dbname=%s user=%s password=%s"
         (getConfig "dbhost") ((forceEither $ CF.get cp "DEFAULT" "dbport")::Integer)
         (getConfig "dbname") (getConfig "dbuser") (getConfig "dbpass")
  let webPort = forceEither $ CF.get cp "DEFAULT" "webport"
  socket <- openSocket webPort $ getConfig "webprocessuser"
  --restart cache
  x <- openLocalState g0
  --init template repo
  ehs <- initTemplateRepo $ getConfig "templatedir"
  either 
    (error . concat) --output init template errors
    (serve socket webPort . 
     capaApp 
       x 
       connString
       (authenticatedForRegister authUriBase)
       (resolveCoop authUriBase x)) --provide auth handlers
    ehs 

main :: IO ()
main = 
  --log unhandled exceptionsx
  run `EX.catch` 
    (\e -> infoM "main" $ show ("Server quit due to: ", e::EX.SomeException))