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
import Happstack.Lite(method, dir, path, Method(..), nullDir)
import Control.Monad(msum)
import Happstack.Server(dirs, decodeBody, defaultBodyPolicy)

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

---------------ENTRY---------------------------------
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
           expireSession ref >> redirect loginUrl
       , dir "export" $ templateResponse "export"
       , redirect loginUrl]
  
  --service router
  , dir "financial" $ dir "results" $ msum [   -- change to api/  
       method GET >> (withCn $ getAllFinancialResultsDetail ref)
     , method POST >> (withCn $ putFinancialResults ref)]
  , dir "allocation" $ msum [
         nullDir >> method GET >> (withCn $ getAllocation ref)
       , dir "disburse" $ dir "actions" $ method GET >> 
           (withCn $ getAllocationDisbursals ref)]
  , dir "members" $ msum [
        nullDir >> method GET >> (withCn $ getMembers ref)
      , dir "equity" $ msum [
           dir "accounts" $ 
             method GET >> (withCn $ getAllMembersEquityAccounts ref)]
      , dir "patronage" $ method GET >> (withCn $ getAllMemberPatronage ref)]
  , dir "member" $ msum [ 
         method POST >> (withCn $ putMemberAndAccounts ref)
       , method GET >> (withCn $ getMember ref)
       , method POST >> (withCn $ putMemberPatronage ref)
       , dir "equity" $ msum [
           dir "disburse" $ method POST >> (withCn $ postScheduleAllocateDisbursal ref)
         , dir "history" $ method POST >> (withCn $ putEquityAction ref)
         , dir "account" $ msum [
                nullDir >> method GET >> (withCn $ getMemberEquityAccount ref)
              , dir "actions" $ 
                  method GET >> (withCn $ getActionsForMemberEquityAcct ref)] ] ]
  , dir "equity" $ msum [
       dir "members" $ msum [
         dir "allocate" $ msum [
             dir "generate" $ method POST >> (withCn $ postAllocateToMembers ref)
           , dir "save" $ method POST >> (withCn $ postAllocationDisbursal ref)] ] ]
  , dir "coop" $ msum [
       nullDir >> method GET >> (withCn $ getCooperative ref)
     , nullDir >> method POST >> (withCn $ putCooperative ref)
     , dir "settings" $ msum [
          dir "allocate" $ msum [ 
               method POST >> (withCn $ putCoopAllocateSettings ref)
             , dir "method" $ method GET >> (withCn $ getAllocMethodDetail ref)
             , dir "seniority" $ dir "levels" $ 
                 method GET >> (withCn $ getSeniorityMappings ref)] 
        , dir "disburse" $ dir "schedule" $ dir "default" $  
                 method POST >> (withCn $ putDefaultDisbursalSchedule ref)
               , method GET >> (withCn $ getDefaultDisbursalSchedule ref)] ] 
  , dir "fiscal" $ dir "periods" $ (withCn $ getLatestFiscalPeriods ref)
  , dir "allocate" $ msum [
      dir "method" $ path $ (okJSResp . fieldDetails . read) -- GET
    , dir "methods" $ method GET >> (okJSResp $ fmap show allocMethods)]
  , dir "export.zip" $ method GET >> (withCn $ exportAll ref)
  , dir "logout" $ expireSession ref]

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
  ehs <- initTemplateRepo (getConfig "templatedir")
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