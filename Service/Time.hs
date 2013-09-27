module Service.Time
where

import Types
import Utils
import Persist
import Domain
import Serialize

import Happstack.Lite 
  (path, dir, ServerPart(..)) 
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

-- for provided day, provide 2 years back and forward
-- should take offset back or forward
getLatestFiscalPeriods :: PersistConnection -> PG.Connection -> ServerPartR
getLatestFiscalPeriods ref dbCn = do
  cpId <- getSessionCoopId ref
  Cooperative{fiscalCalendarType=ft} <- liftIO $ coopGet dbCn cpId
  let FiscalCalendarType{startf=startMonth, periodTypef=pt} = ft
  today <- liftIO getCurrentDay
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
