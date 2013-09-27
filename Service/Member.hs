{-# Language ScopedTypeVariables #-}
module Service.Member
where

import Types
import Utils
import Persist
import Domain
import Serialize

import Happstack.Lite (path, dir, ServerPart(..), lookBS) 
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

putMemberAndAccounts :: PersistConnection -> PG.Connection -> ServerPartR
putMemberAndAccounts ref dbCn = do 
  cpId <- getSessionCoopId ref
  mem <- parseObject 
  (liftIO $ do 
    mbrId <- mbrSave dbCn cpId mem
    acctSaveDefault dbCn cpId mbrId) >>= okJSResp
     
getMember :: PersistConnection -> PG.Connection -> ServerPartR
getMember ref dbCn =
  path $ \(mid::Integer) -> do
    cpId <- getSessionCoopId ref
    (liftIO $ mbrGet dbCn cpId mid) >>= okJSResp

getMembers :: PersistConnection -> PG.Connection -> ServerPartR
getMembers ref dbCn = do 
  cpId <- getSessionCoopId ref
  (liftIO $ getCurrentDay >>= mbrGetAll dbCn cpId) >>= okJSResp
