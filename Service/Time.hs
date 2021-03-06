module Service.Time
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Service.Security

getLatestFiscalPeriods :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getLatestFiscalPeriods = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    (Cooperative{fiscalCalendarType=ft}, today) <- 
      liftIO $ (,) <$> runReaderT coopGet (dbCn, cpId) <*> getCurrentDay
    okJSResp $ enumeratePeriods ft today
