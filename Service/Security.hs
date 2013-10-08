{-# Language OverloadedStrings #-}
module Service.Security
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize

import Happstack.Lite 
  (ok, path, dir, Response(..), ServerPart(..), lookBS, lookCookieValue,
   toResponseBS) 
import qualified Data.Maybe as MB
import qualified Data.List as L            
import Data.Time (fromGregorian , toGregorian, UTCTime(..), getCurrentTime,
                  addGregorianMonthsClip, addGregorianYearsClip)   
import Data.Map as M
import Data.Default
import Data.Aeson (encode, decode)
import Control.Monad.IO.Class (liftIO)  
import qualified Data.ByteString.Lazy.Char8 as LB 
import qualified Data.ByteString.Char8 as CB 

import qualified Database.HDBC.PostgreSQL as PG -- remove me

import Control.Monad(guard, void)

import System.Log.Logger as LG
import Text.Printf(printf)

import Happstack.Lite
  (mkCookie, addCookies, expireCookie, lookCookieValue, CookieLife(Session))
import qualified Data.Time as CK
import System.Locale(defaultTimeLocale)
import Data.Acid.Advanced   ( query', update' )

import qualified Data.Aeson.Types as AT
import qualified Data.Aeson as A
import Network.HTTP.Conduit(simpleHttp)
import Happstack.Server(look)

getSessionCoopId :: PersistConnection -> ServerPart Integer
getSessionCoopId ref = do 
  Globals{sessions=sessions} <- query' ref GetIt
  sessionId <- lookCookieValue "sessionId"
  let res = M.lookup sessionId sessions
  guard $ MB.isJust res
  let Just (_,cpId) = res
  return cpId

expireSession :: PersistConnection -> ServerPartR
expireSession ref = do
  g@Globals{sessions=sessions} <- query' ref GetIt
  sessionId <- lookCookieValue "sessionId"
  void $ update' ref $ PutIt g{sessions = M.delete sessionId sessions}
  expireCookie "sessionId"
  okJSResp ()

attachSession :: PersistConnection -> Cooperative -> ServerPart ()
attachSession ref Cooperative{cooperativeId=cpId, name=name, username=user} = do
  liftIO $ LG.infoM "Service.attachSession" $ printf "Resolved for %s, %s" user name
  utcNow <- liftIO CK.getCurrentTime
  let secs = CK.formatTime defaultTimeLocale "%s" utcNow
  let sessionId = secs
  addCookies [(Session, mkCookie "sessionId" sessionId)]
  g@Globals{sessions=sessions} <- query' ref GetIt
  void $ update' ref $ PutIt g{sessions = M.insert sessionId (user, cpId) sessions}


retrieveProfile :: String -> ServerPart OpenID
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
