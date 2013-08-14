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
                     
generalTemplateResponse :: 
  PG.Connection -> TemplateStore -> PersistConnection -> 
  Bool -> B.ByteString -> ServerPartR
generalTemplateResponse dbCn hState ref secure name = 
  nullDir >> method GET >> do 
    (_, _, cookies) <- askRqEnv
    let mbSession = lookup "sessionid" cookies
    liftIO $ LG.debugM "yo" $ show cookies
    if secure && MB.isNothing mbSession
      then seeOther ("/control/enter"::String) (toResponse ())
      else do 
        --(alloc, disb) <- getCoopRegistrationState ref dbCn
        --if not alloc || not disb
        --then seeOther ("/control/coop/register/partial?alloc=t"
        let rendered = runIdentity $ renderTemplate hState name
        maybe (notFound $ toResponse $ "Template not found: " ++ B.unpack name)
  	      (\(bldr,_) -> ok $ toResponse $ toByteString bldr)
	      rendered  	
     

resolveCoop :: String -> PG.Connection -> PersistConnection -> ServerPartR
resolveCoop authUriBase dbCn ref = do 
  ident <- retrieveProfile authUriBase
  mbCoop <- liftIO $ coopGetFor dbCn ident
  let redir = maybe "/control/enter" (\_ -> "/control/coop/summary") mbCoop
  (if MB.isJust mbCoop then
     attachSession ref $ MB.fromJust mbCoop
   else 
     liftIO $ LG.errorM "resolveCoop" $ printf "%s no coop" ident)
  seeOther (redir::String) (toResponse ()) 
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
  seeOther ("/control/coop/register?username=" ++ ident) (toResponse ()) --url escape

---------------ENTRY---------------------------------
capaApp :: 
  PersistConnection -> PG.Connection -> [ServerPartR] -> TemplateStore -> ServerPartR
capaApp ref conn [resolveCoopCtrl, authControl] hState =
  let templateResponseWithState = generalTemplateResponse conn hState ref
      unsecuredTemplateResponse = templateResponseWithState False
      templateResponse = templateResponseWithState True
  in msum [
    dir "img" $ serveDirectory EnableBrowsing [] "control/images"
    --js here?
    --partial path failurs like missing parameter?
  , dir "control" $ msum [ --remove control 
         dir "coop" $ msum [
            dir "summary" $ templateResponse "coopSummary"
          , dir "register" $ msum [
                nullDir >> unsecuredTemplateResponse "registerCoop"
              , dir "partial" $ templateResponse "partialRegistration"
              , dir "authenticate" $ unsecuredTemplateResponse "registerAuthenticate"]
          , dir "settings" $ msum [
              templateResponse "coopSettings"
            , dir "disburse" $ dir "schedule" $ 
                  templateResponse "setDefaultDisbursalSchedule"
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
           expireSession ref >> seeOther ("/control/enter"::String) (toResponse ())
       , dir "export" $ templateResponse "export"]
  
  , dir "financial" $ dir "results" $ msum [   -- change to api/  
       method GET >> getAllFinancialResultsDetail ref conn
     , method POST >> putFinancialResults ref conn]
  , dir "members" $ msum [
        nullDir >> method GET >> getMembers ref conn
      , dir "equity" $ dir "accounts" $ 
          method GET >> getAllMembersEquityAccounts ref conn
      , dir "patronage" $ method GET >> getAllMemberPatronage ref conn]
  , dir "member" $ msum [ 
         method POST >> putMember ref conn
       , method GET >> getMember ref conn
       , method POST >> putMemberPatronage ref conn
       , dir "equity" $ msum [
           dir "disburse" $ method POST >> postScheduleAllocateDisbursal ref conn
         , dir "history" $ method POST >> putEquityAction ref conn
         , dir "account" $ msum [
                nullDir >> method GET >> getMemberEquityAccount ref conn
              , dir "actions" $ 
                  method GET >> getActionsForMemberEquityAcct ref conn] ] ]
  , dir "equity" $ msum [
       dir "members" $ msum [
         dir "allocate" $ msum [
             dir "generate" $ method POST >> postAllocateToMembers ref conn 
           , dir "save" $ method POST >> postAllocationDisbursal ref conn] ] ]
  , dir "coop" $ msum [
       nullDir >> method GET >> getCooperative ref conn  
     , nullDir >> method POST >> putCooperative ref conn
     , dir "settings" $ msum [
          dir "allocate" $ msum [ 
               method POST >> putCoopAllocateSettings ref conn
             , dir "method" $ method GET >> getAllocMethodDetail ref conn
             , dir "seniority" $ dir "levels" $ 
                 method GET >> getSeniorityMappings ref conn] 
        , dir "disburse" $ dir "schedule" $ dir "default" $  
                 method POST >> putDefaultDisbursalSchedule ref conn
               , method GET >> getDefaultDisbursalSchedule ref conn] ] 
  , dir "fiscal" $ dir "periods" $ getLatestFiscalPeriods ref conn  
  , dir "allocate" $ msum [
      dir "method" $ path $ (okJSResp . fieldDetails . read) -- GET
    , dir "methods" $ method GET >> (okJSResp $ fmap show allocMethods)]
  , dir "export.zip" $ method GET >> exportAll ref conn
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
  -- config should select debug or not
  x <- openLocalState g0
  conn <- 
     PG.connectPostgreSQL 
       (printf "host=%s port=%d dbname=%s user=%s password=%s"
              dbHost dbPort dbName dbUser dbPass)
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
       conn 
       [resolveCoop authUriBase conn x, authenticatedForRegister authUriBase])
    ehs 
  DB.disconnect conn


