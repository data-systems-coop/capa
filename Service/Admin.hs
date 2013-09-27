module Service.Admin
where

import Types
import Utils
import Persist
import Domain
import Serialize

import Happstack.Lite (ok, path, dir, ServerPart(..), lookBS, toResponseBS) 
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

import qualified Data.ByteString.Char8 as CB 
import qualified Codec.Archive.Zip as ZP
import qualified System.Posix.Directory as DR
import qualified System.Posix.Files as FL
import qualified System.Process as SP

import Service.Security

--CHANGE TO CLIENT SIDE CREATION IN MEMORY
exportAll :: PersistConnection -> PG.Connection -> ServerPartR
exportAll ref dbCn = do
  cpId <- getSessionCoopId ref
  UTCTime{utctDayTime=time1} <- liftIO getCurrentTime
  let time = show time1
  arch <- liftIO $ do 
    DR.createDirectory ("/tmp/" ++ time) FL.accessModes
    SP.system ("chmod 777 /tmp/" ++ time) 
    acnExportFor dbCn cpId ("/tmp/" ++ time ++ "/capaActions.csv") 
    ptrngExportFor dbCn cpId ("/tmp/" ++ time ++ "/capaPatronage.csv")
    rsltExportFor dbCn cpId ("/tmp/" ++ time ++ "/capaResults.csv")
    stngExportFor dbCn cpId ("/tmp/" ++ time ++ "/capaSettings.csv")
    ZP.addFilesToArchive [ZP.OptRecursive] ZP.emptyArchive [("/tmp/" ++ time)] 
  ok $ toResponseBS (CB.pack "application/zip") $ ZP.fromArchive arch

