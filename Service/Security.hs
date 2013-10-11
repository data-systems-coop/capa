{-# Language OverloadedStrings #-}
module Service.Security
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Data.Map as M
import Happstack.Lite
  (mkCookie, addCookies, expireCookie, lookCookieValue, CookieLife(Session))
import qualified Data.Time as CK
import System.Locale(defaultTimeLocale)
import Data.Acid.Advanced   ( query', update' )

import qualified Data.Aeson.Types as AT
import qualified Data.Aeson as A
import Network.HTTP.Conduit(simpleHttp)


getSessionCoopId :: ReaderT PersistConnection (ServerPartT IO) Integer
getSessionCoopId = do 
  ref <- ask 
  lift $ do 
    Globals{sessions=sessions} <- query' ref GetIt  -- get all sesssion
    sessionId <- lookCookieValue "sessionId" -- get users session id
    let res = M.lookup sessionId sessions  -- attempt find session
    guard $ isJust res -- ensure found
    let Just (_,cpId) = res -- retrieve id from it
    return cpId -- 

expireSession :: ReaderT PersistConnection (ServerPartT IO) Response
expireSession = do
  ref <- ask 
  lift $ do 
    g@Globals{sessions=sessions} <- query' ref GetIt
    sessionId <- lookCookieValue "sessionId"
    void $ update' ref $ PutIt g{sessions = M.delete sessionId sessions}
    expireCookie "sessionId"
    okJSResp ()

attachSession :: Cooperative -> ReaderT PersistConnection (ServerPartT IO) ()
attachSession Cooperative{cooperativeId=cpId, name=name, username=user} = do
  ref <- ask
  lift $ do 
    liftIO $ infoM "Service.attachSession" $ printf "Resolved for %s, %s" user name
    utcNow <- liftIO CK.getCurrentTime
    let secs = CK.formatTime defaultTimeLocale "%s" utcNow
    let sessionId = secs
    addCookies [(Session, mkCookie "sessionId" sessionId)]
    g@Globals{sessions=sessions} <- query' ref GetIt
    void $ update' ref $ PutIt g{sessions = M.insert sessionId (user, cpId) sessions}


retrieveProfile :: String -> ServerPartT IO OpenID
retrieveProfile authUriBase = do 
  --handle reply
  token <- look "token"
  let reqUri = authUriBase ++ token
  --retrieve profile
  r <- liftIO $ simpleHttp reqUri
  --parse response
  let Just (A.Object r2) = A.decode r
  let AT.Success pf = (AT.parse (A..: "profile") r2)::AT.Result A.Object
  let AT.Success ident = (AT.parse (A..: "identifier") pf)::AT.Result String
  return ident

resolveCoop :: 
  String -> PersistConnection -> String -> String -> Connection -> ServerPartR
resolveCoop authUriBase ref loginUrl homeUrl dbCn = do 
  -- get profile
  ident <- retrieveProfile authUriBase 
  -- check for coop
  mbCoop <- liftIO $ coopGetFor dbCn ident
  let redir = maybe loginUrl (\_ -> homeUrl) mbCoop
  (if isJust mbCoop then
     -- if has coop, start session
     runReaderT (attachSession $ fromJust mbCoop) ref
   else 
     -- log failed attempt to find coop for profile
     liftIO $ errorM "resolveCoop" $ printf "%s no coop" ident)
  redirect redir
    -- then redirect to login screen with "Not associated with coop"
  

