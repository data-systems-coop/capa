module Service.Cooperative 
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Service.Security

getCooperative :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getCooperative = do
  cpId <- withReaderT fst getSessionCoopId   
  dbCn <- asks snd
  c <- liftIO $ runReaderT coopGet (dbCn, cpId)
  lift $ okJSResp c

putCooperative :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
putCooperative = do
  coop <- lift $ parseObject  
  dbCn <- asks snd
  coop <- liftIO $ do 
    cpId <- runReaderT (coopSave coop) dbCn
    runReaderT coopGet (dbCn, cpId)
  withReaderT fst $ attachSession coop
  lift $ okJSResp ()
