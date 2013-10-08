{-# Language ScopedTypeVariables #-}
module Service.FinancialResults
where

import Types
import Utils
import Persist.Persist
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

getAllFinancialResultsDetail :: PersistConnection -> PG.Connection -> ServerPartR
getAllFinancialResultsDetail ref dbCn = do 
  cpId <- getSessionCoopId ref
  (liftIO $ rsltGetAll dbCn cpId) >>= okJSResp

       
putFinancialResults :: PersistConnection -> PG.Connection -> ServerPartR
putFinancialResults ref dbCn = do 
  cpId <- getSessionCoopId ref
  surplus <- lookRead "surplus"
  overStr <- lookBS "over"
  let Just over = decode overStr
  let res = FinancialResults over surplus Nothing
  (liftIO $ rsltSaveFor dbCn cpId res) >>= okJSResp
