{-# Language OverloadedStrings #-}
module Main (main) where

import Types
import Utils
import Domain
import Serialize
import Persist.Persist
import Service.Service
import View

import Happstack.Lite
   (method, dir, path, 
    Method(..), nullDir, ToMessage(..), seeOther,
    CookieLife(Session), mkCookie, addCookies, expireCookie, lookCookieValue, 
    ServerPart)
import Control.Monad(msum)
import Happstack.Server.Routing (dirs)
import Happstack.Server(decodeBody, defaultBodyPolicy, simpleHTTPWithSocket, nullConf,
                        bindPort, Conf(..))

import Control.Monad.IO.Class (liftIO)
 
import qualified Data.ByteString.Char8 as B  -- + templates


import qualified Control.Exception as EX --util
import Data.Acid(openLocalState)

import qualified Database.HDBC.PostgreSQL as PG -- remove me
import Database.HDBC(disconnect)

import Data.Either.Utils (forceEither)
import qualified Data.ConfigFile as CF

import qualified System.Log.Handler.Syslog as SYS

import Happstack.Server.RqData(HasRqData(..), RqEnv)
import Happstack.Lite(serveDirectory, Browsing(..))

import System.Posix.User (setUserID, UserEntry(..), getUserEntryForName)

--------------APP CONTROLLER------------------------
                     
redirect :: String -> ServerPartR
redirect url = seeOther url $ toResponse ()


  

generalTemplateResponse :: 
  TemplateStore -> PersistConnection -> Bool -> Bool -> 
  B.ByteString -> PG.Connection -> ServerPartR
generalTemplateResponse hState ref secure checkReg name dbCn = 
  nullDir >> method GET >> do 
    -- lookup cookie for sessionid, if any
    (_, _, cookies) <- askRqEnv
    let mbSession = lookup "sessionid" cookies --lookCookieValue
    -- based on secure or not x session x check registration status
        -- either redirect to login, partially registered, or page requested
    if secure && isNothing mbSession
      then redirect "/control/enter"
      else do 
        if checkReg
           then do 
             (alloc, disb) <- getCoopRegistrationState ref dbCn
             if (not alloc || not disb) 
               then
                 redirect $ 
                   printf "/control/coop/register/partial?alloc=%s&disburse=%s" 
                     (show alloc) (show disb) 
               else
                 templateFor hState name  
           else
             templateFor hState name

resolveCoop :: String -> PersistConnection -> PG.Connection -> ServerPartR
resolveCoop authUriBase ref dbCn = do 
  -- get profile
  ident <- retrieveProfile authUriBase 
  -- check for coop
  mbCoop <- liftIO $ coopGetFor dbCn ident
  -- homepage or login page
  let redir = maybe "/control/enter" (\_ -> "/control/coop/summary") mbCoop
  (if isJust mbCoop then
     -- if has coop, start session
     attachSession ref $ fromJust mbCoop
   else 
     -- log failed attempt to find coop for profile
     liftIO $ errorM "resolveCoop" $ printf "%s no coop" ident)
  redirect redir
    -- then redirect to login screen with "Not associated with coop"
  
authenticatedForRegister :: String -> ServerPartR
authenticatedForRegister authUriBase = do 
  retrieveProfile authUriBase >>= 
    redirect . ("/control/coop/register?username=" ++) --url escape


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
  PersistConnection -> String -> [ServerPartR] -> TemplateStore -> ServerPartR
capaApp ref connStr [resolveCoopCtrl, authControl] hState =
 let withCn = withConn connStr
     templateResponseWithState = generalTemplateResponse hState ref
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
                nullDir >> unsecuredTemplateResponse "registerCoop"
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
           , dir "register" $ authControl]
       , dir "logout" $ 
           expireSession ref >> redirect "/control/enter"
       , dir "export" $ templateResponse "export"
       , redirect "/control/enter"]
  
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


-- Main.hs
run :: IO ()  
run = do
  --receive config file arg
  --setup logger
  s <- SYS.openlog "capa" [] SYS.USER DEBUG
  updateGlobalLogger rootLoggerName (addHandler s . setLevel DEBUG)
  infoM "main" "started"
  --read config
  val <- CF.readfile CF.emptyCP "etc/dev.txt"
  let cp = forceEither val
  let getConfig = forceEither . CF.get cp "DEFAULT" 
  -- config should select debug or not  
  --read db config
  let dbHost = (getConfig "dbhost")::String
  let dbPort = (forceEither $ CF.get cp "DEFAULT" "dbport")::Integer
  let dbName = (getConfig "dbname")::String
  let dbUser = (getConfig "dbuser")::String
  let dbPass = (getConfig "dbpass")::String
  --read authent config
  let authUriBase = (getConfig "authuribase")::String
  --read service config
  let servicesUri = (getConfig "servicesuri")::String
  --read template config
  let templateDir = (getConfig "templatedir")::String
  --build db string
  let connString = 
        (printf "host=%s port=%d dbname=%s user=%s password=%s"
              dbHost dbPort dbName dbUser dbPass)
  --read / build web server config
  let conf = nullConf { port = forceEither $ CF.get cp "DEFAULT" "webport"}
  --build server socket
  socket <- bindPort conf
  --switch user
  getUserEntryForName (getConfig "webprocessuser") >>= setUserID . userID
  --restart cache
  x <- openLocalState g0
  --init template repo
  ehs <- initTemplateRepo templateDir 
  either 
    (error . concat) --output init template errors
    --start server
    (simpleHTTPWithSocket socket conf . 
     capaApp 
       x 
       connString
       [withConn connString $ resolveCoop authUriBase x, --provide auth handlers
        authenticatedForRegister authUriBase])
    ehs 

main :: IO ()
main = 
  --log unhandled exceptionsx
  run `EX.catch` 
    (\e -> infoM "main" $ show ("Server quit due to: ", e::EX.SomeException))