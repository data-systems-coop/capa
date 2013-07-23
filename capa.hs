{-# Language OverloadedStrings #-}
module Main (main) where

import Types
import Utils
import Domain
import Serialize
import Persist
import Service

import Happstack.Lite
   (ok, method, serve, dir, Method(..), nullDir, notFound, ToMessage(..), seeOther,
    CookieLife(Session), mkCookie, addCookies, expireCookie, lookCookieValue)
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



--------------APP CONTROLLER------------------------
type TemplateStore = HeistState Identity
                     
--expose default templateResponse with check==True                     
                     
templateResponse name hState =  -- parameter to check cookie or not
  nullDir >> method GET >>
  --if check, then check cookie is set
  let rendered = runIdentity $ renderTemplate hState name
  in maybe (notFound $ toResponse $ "Template not found: " ++ B.unpack name)
  	   (\(bldr,_) -> ok $ toResponse $ toByteString bldr)
	   rendered  	
     

resolveCoop authUriBase ref = do 
  token <- lookString "token"
  let reqUri = authUriBase ++ token
  r <- liftIO $ simpleHttp reqUri
  let Just (A.Object r2) = A.decode r
  let AT.Success pf = (AT.parse (A..: "profile") r2)::AT.Result A.Object
  let AT.Success ident = (AT.parse (A..: "identifier") pf)::AT.Result String
  Globals{cooperative=Cooperative{username=user}} <- query' ref GetIt
  let redir = if user == ident then "/control/financial/results" else "/control/enter"
  (if (user == ident) then do
      liftIO $ LG.infoM "resolveCoop" $ printf "%s resolved for %s" user ("c1"::String)
      utcNow <- liftIO CK.getCurrentTime
      let secs = CK.formatTime defaultTimeLocale "%s" utcNow
      let sessionId = secs
      addCookies [(Session, mkCookie "sessionId" sessionId)]
      g@Globals{sessions=sessions} <- query' ref GetIt
      void $ update' ref $ PutIt g{sessions = M.insert sessionId (ident,1) sessions}
   else 
     liftIO $ LG.errorM "resolveCoop" $ printf "%s no coop" ident)
  seeOther (redir::String) (toResponse ()) 
    -- then redirect to login screen with "Not associated with coop"

  
---------------ENTRY---------------------------------
capaApp :: 
  PersistConnection -> 
  PG.Connection -> 
  (PersistConnection -> ServerPartR) ->
  TemplateStore -> 
  ServerPartR
capaApp ref conn resolveCoopCtrl hState = msum [
    --partial path failurs like missing parameter?
    dir "control" $ msum [
         dir "coop" $ msum [
            dir "summary" $ templateResponse "coopSummary" hState
          , dir "register" $ templateResponse "registerCoop" hState
          , dir "settings" $ templateResponse "coopSettings" hState ] 
             -- view all, update alloc, update disbursal
       , dir "member" $ msum [
            dir "accounts" $ templateResponse "memberAccounts" hState
          , dir "account" $ dir "action" $ dir "add" $ 
              templateResponse "addAction" hState
          , dir "patronage" $ dir "record" $ 
              templateResponse "recordPatronage" hState ]
       , dir "members" $ msum [
             dir "patronage" $ dir "period" $ templateResponse "periodPatronage" hState
           , dir "add" $ templateResponse "newMember" hState ]
       , dir "financial" $ dir "results" $ msum [
             templateResponse "financialResults" hState
           , dir "record" $ templateResponse "recordResult" hState ]
       , dirs "equity" $ msum [ 
            dir "members"  $ dir "allocationsDisbursals" $ 
              templateResponse "allocationsDisbursals" hState ] 
       , dir "enter" $ templateResponse "enter" hState 
       , dir "login" $ dir "resolve" $ dir "coop" $ method POST >> resolveCoopCtrl ref]
  
  , dir "financial" $ dir "results" $ msum [ 
       method GET >> getAllFinancialResultsDetail ref conn
     , method POST >> putFinancialResults ref conn]
  , dir "members" $ msum [
        nullDir >> method GET >> getMembers ref conn
      , dir "patronage" $ method GET >> getAllMemberPatronage ref conn]
  , dir "member" $ msum [ 
         method POST >> putMember ref
       , method GET >> getMember ref
       , method POST >> putMemberPatronage ref conn
       , dir "equity" $ msum [
           dir "disburse" $ method POST >> postScheduleAllocateDisbursal ref conn
         , dir "history" $ method POST >> putEquityAction ref ] ]
  , dir "equity" $ msum [
       dir "members" $ msum [
         dir "allocate" $ msum [
             dir "generate" $ method POST >> postAllocateToMembers ref 
           , dir "save" $ method POST >> postAllocationDisbursal ref conn] ] ]
  , dir "coop" $ msum [
       dir "settings" $ msum [
          dir "allocate" $ method POST >> putCoopAllocateSettings ref] ]
  , dir "fiscal" $ dir "periods" $ getLatestFiscalPeriods ref]
                     
main = do
  s <- SYS.openlog "capa" [] SYS.USER LG.DEBUG
  LG.updateGlobalLogger LG.rootLoggerName (LG.addHandler s . LG.setLevel LG.INFO)
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
       (resolveCoop authUriBase))
    ehs 
  DB.disconnect conn


