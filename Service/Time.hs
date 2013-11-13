module Service.Time
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Data.Time (fromGregorian , toGregorian, UTCTime(..), getCurrentTime,
                  addGregorianMonthsClip, addGregorianYearsClip)   

import Service.Security

-- for provided day, provide 2 years back and forward
-- should take offset back or forward
getLatestFiscalPeriods :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getLatestFiscalPeriods = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    Cooperative{fiscalCalendarType=ft} <- liftIO $ runReaderT coopGet (dbCn, cpId)
    let FiscalCalendarType{startf=startMonth, periodTypef=pt} = ft
    today <- liftIO getCurrentDay
    -- move below into Domain
    let (endYear,_,_) = toGregorian $ addGregorianYearsClip 5 today
    let end = fromGregorian endYear startMonth 1
    let stepBack = 
         if pt == Year 
         then addGregorianYearsClip (-1)
         else addGregorianMonthsClip (-3)
    let enumStarts d = d : (enumStarts $ stepBack d)
    let periods = 
         fmap 
          ((\(yr,mo,_) -> FiscalPeriod (GregorianMonth yr mo) pt) . toGregorian)
          (take 36 $ enumStarts end)
    okJSResp periods
