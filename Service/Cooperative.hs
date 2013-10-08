module Service.Cooperative 
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize

import Happstack.Lite 
  (ok, path, dir, Response(..), ServerPart(..), ToMessage(..)) 
import qualified Data.Maybe as MB
import qualified Data.List as L            
import Data.Time (fromGregorian , toGregorian, UTCTime(..), getCurrentTime,
                  addGregorianMonthsClip, addGregorianYearsClip)   
import Data.Map as M
import Data.Default
import Data.Aeson (encode, decode)
import Control.Monad.IO.Class (liftIO)  

import qualified Database.HDBC.PostgreSQL as PG -- remove me

import Control.Monad(guard, void)

import System.Log.Logger as LG
import Text.Printf(printf)

import Service.Security

getCooperative :: PersistConnection -> PG.Connection -> ServerPartR
getCooperative ref dbCn = do
  cpId <- getSessionCoopId ref
  (liftIO $ coopGet dbCn cpId) >>= okJSResp

putCooperative :: PersistConnection -> PG.Connection -> ServerPartR
putCooperative ref dbCn = do
  coop <- parseObject  
  coop <- liftIO $ do 
    cpId <- coopSave dbCn coop 
    coopGet dbCn cpId
  attachSession ref coop
  okJSResp ()
