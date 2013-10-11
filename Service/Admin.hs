module Service.Admin
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Data.Time (UTCTime(..), getCurrentTime)

import qualified Data.ByteString.Char8 as CB 
import qualified Codec.Archive.Zip as ZP
import qualified System.Posix.Directory as DR
import qualified System.Posix.Files as FL
import qualified System.Process as SP

import Service.Security

--CHANGE TO CLIENT SIDE CREATION IN MEMORY
exportAll :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
exportAll = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
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
  lift $ ok $ toResponseBS (CB.pack "application/zip") $ ZP.fromArchive arch

