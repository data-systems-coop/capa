{-# Language OverloadedStrings #-}
module Main (main) where

import Types
import Utils
import Domain
import Serialize
import Persist
import Service

import Happstack.Lite
   (ok, method, serve, dir, path, 
    Method(..), nullDir, notFound, ToMessage(..), seeOther,
    CookieLife(Session), mkCookie, addCookies, expireCookie, lookCookieValue, 
    ServerPart)
import Happstack.Server.Routing (dirs)

import Network.HTTP.Conduit(simpleHttp)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Generic as AG 
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Aeson.Types as AT
import Data.Acid.Advanced (query')

import Text.Blaze.Html5 (html, p, toHtml) -- present, html, temlate
import Blaze.ByteString.Builder (toByteString)
import qualified Data.ByteString.Char8 as B  -- + templates
import Heist (loadTemplates, HeistConfig(..), HeistState, initHeist,
              defaultLoadTimeSplices, defaultInterpretedSplices)
import Heist.Interpreted (renderTemplate)
import Control.Monad.Trans.Either
import Control.Monad.Identity

import Control.Exception(bracket)     -- util
import Data.Acid(openLocalState)

import Control.Monad(when)
import qualified Data.Time as CK
import System.Locale(defaultTimeLocale)
import Data.Acid.Advanced   ( query', update' )
import qualified Data.Map as M

import qualified Database.HDBC.PostgreSQL as PG -- remove me
import qualified Database.HDBC as DB

import Data.Either.Utils (forceEither)
import qualified Data.ConfigFile as CF
import Text.Printf(printf)

import qualified System.Log.Logger as LG
import qualified System.Log.Handler.Syslog as SYS

import qualified Data.Maybe as MB

import Happstack.Server.RqData(HasRqData(..), RqEnv)
import Happstack.Lite(serveDirectory, Browsing(..))

--------------APP CONTROLLER------------------------
type TemplateStore = HeistState Identity
                     
redirect :: String -> ServerPartR
redirect url = seeOther url $ toResponse ()

generalTemplateResponse :: 
  TemplateStore -> PersistConnection -> Bool -> Bool -> 
  B.ByteString -> PG.Connection -> ServerPartR
generalTemplateResponse hState ref secure checkReg name dbCn = 
  nullDir >> method GET >> do 
    (_, _, cookies) <- askRqEnv
    let mbSession = lookup "sessionid" cookies
    if secure && MB.isNothing mbSession
      then redirect "/control/enter"
      else do 
        (alloc, disb) <- getCoopRegistrationState ref dbCn
        if checkReg && (not alloc || not disb) 
          then
            redirect $ 
              printf "/control/coop/register/partial?alloc=%s&disburse=%s" 
                (show alloc) (show disb) 
          else do              
            let rendered = runIdentity $ renderTemplate hState name
            maybe (notFound $ toResponse $ "Template not found: " ++ B.unpack name)
  	          (\(bldr,_) -> ok $ toResponse $ toByteString bldr)
	          rendered  	
     

resolveCoop :: String -> PersistConnection -> PG.Connection -> ServerPartR
resolveCoop authUriBase ref dbCn = do 
  ident <- retrieveProfile authUriBase
  mbCoop <- liftIO $ coopGetFor dbCn ident
  let redir = maybe "/control/enter" (\_ -> "/control/coop/summary") mbCoop
  (if MB.isJust mbCoop then
     attachSession ref $ MB.fromJust mbCoop
   else 
     liftIO $ LG.errorM "resolveCoop" $ printf "%s no coop" ident)
  redirect redir
    -- then redirect to login screen with "Not associated with coop"

retrieveProfile :: String -> ServerPart OpenID
retrieveProfile authUriBase = do 
  token <- lookString "token"
  let reqUri = authUriBase ++ token
  r <- liftIO $ simpleHttp reqUri
  let Just (A.Object r2) = A.decode r
  let AT.Success pf = (AT.parse (A..: "profile") r2)::AT.Result A.Object
  let AT.Success ident = (AT.parse (A..: "identifier") pf)::AT.Result String
  return ident
  
authenticatedForRegister :: String -> ServerPartR
authenticatedForRegister authUriBase = do 
  ident <- retrieveProfile authUriBase
  redirect $ "/control/coop/register?username=" ++ ident --url escape

withConn :: String -> (PG.Connection -> ServerPartR) -> ServerPartR
withConn connString body = do -- use bracket instead
  cn <- liftIO $ PG.connectPostgreSQL connString
  r <- body cn  
  liftIO $ DB.disconnect cn
  return r

---------------ENTRY---------------------------------
capaApp :: 
  PersistConnection -> String -> [ServerPartR] -> TemplateStore -> ServerPartR
capaApp ref connStr [resolveCoopCtrl, authControl] hState =
  let withCn = withConn connStr
      templateResponseWithState = generalTemplateResponse hState ref
      unsecuredTemplateResponse nm = withCn $ templateResponseWithState False True nm
      templateResponse nm = withCn $ templateResponseWithState True True nm
      noPartialTemplateResponse nm = withCn $ templateResponseWithState True False nm
  in msum [
    dir "img" $ serveDirectory EnableBrowsing [] "control/images"
    --js here?
    --partial path failurs like missing parameter?
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
       , dir "export" $ templateResponse "export"]
  
  , dir "financial" $ dir "results" $ msum [   -- change to api/  
       method GET >> (withCn $ getAllFinancialResultsDetail ref)
     , method POST >> (withCn $ putFinancialResults ref)]
  , dir "members" $ msum [
        nullDir >> method GET >> (withCn $ getMembers ref)
      , dir "equity" $ dir "accounts" $ 
          method GET >> (withCn $ getAllMembersEquityAccounts ref)
      , dir "patronage" $ method GET >> (withCn $ getAllMemberPatronage ref)]
  , dir "member" $ msum [ 
         method POST >> (withCn $ putMember ref)
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

                                          
main = do
  s <- SYS.openlog "capa" [] SYS.USER LG.DEBUG
  LG.updateGlobalLogger LG.rootLoggerName (LG.addHandler s . LG.setLevel LG.DEBUG)
  LG.infoM "main" "started"
  val <- CF.readfile CF.emptyCP "etc/dev.txt"
  let cp = forceEither val
  let dbHost = (forceEither $ CF.get cp "DEFAULT" "dbhost")::String
  let dbPort = (forceEither $ CF.get cp "DEFAULT" "dbport")::Integer
  let dbName = (forceEither $ CF.get cp "DEFAULT" "dbname")::String
  let dbUser = (forceEither $ CF.get cp "DEFAULT" "dbuser")::String
  let dbPass = (forceEither $ CF.get cp "DEFAULT" "dbpass")::String
  let authUriBase = (forceEither $ CF.get cp "DEFAULT" "authuribase")::String
  let servicesUri = (forceEither $ CF.get cp "DEFAULT" "servicesuri")::String
  let templateDir = (forceEither $ CF.get cp "DEFAULT" "templatedir")
  let connString = 
        (printf "host=%s port=%d dbname=%s user=%s password=%s"
              dbHost dbPort dbName dbUser dbPass)
  -- config should select debug or not
  x <- openLocalState g0
  ehs <- runEitherT $ do 
     templateRepo <- loadTemplates templateDir
     let hCfg = (HeistConfig 
	           [] 
	           (defaultInterpretedSplices ++ defaultLoadTimeSplices) 
	           [] 
	           [] 
	           templateRepo)::HeistConfig Identity
     initHeist hCfg
  either 
    (error . concat) 
    (serve Nothing . 
     capaApp 
       x 
       connString
       [withConn connString $ resolveCoop authUriBase x, 
        authenticatedForRegister authUriBase])
    ehs 


