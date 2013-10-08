{-# Language ScopedTypeVariables #-}
module Service.WorkPatronage
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
import qualified Data.ByteString.Lazy.Char8 as LB 

import Service.Security

lookPatronage :: AllocationMethod -> FiscalPeriod -> ServerPart WorkPatronage
lookPatronage method performedOver = 
  case method of 
    ProductiveHours -> do
      work <- lookRead "work"
      return def{work=work,performedOver=performedOver}
    Wages -> do
      skillWeightedWork <- lookRead "skillWeightedWork"
      return def{skillWeightedWork=skillWeightedWork,performedOver=performedOver}
    _ -> do
      work <- lookRead "work"
      skillWeightedWork <- lookRead "skillWeightedWork"
      case method of 
        SimpleMix -> 
          return def{work=work,skillWeightedWork=skillWeightedWork,
                     performedOver=performedOver}
        _ -> do
          seniority <- lookRead "seniority"
          return def{work=work,skillWeightedWork=skillWeightedWork,
                               seniority=seniority,performedOver=performedOver}

putMemberPatronage :: PersistConnection -> PG.Connection -> ServerPartR
putMemberPatronage ref dbCn = 
  path $ \(idIn::Integer) -> dir "patronage" $ path $ \(performedOverStr::String) -> do
        cpId <- getSessionCoopId ref
	let Just performedOver = decode $ LB.pack performedOverStr        
        (allocMethod, _) <- liftIO $ allocStngGet dbCn cpId
        lookPatronage allocMethod performedOver >>= 
          (liftIO . ptrngSaveFor dbCn cpId idIn) >>= okJSResp

getAllMemberPatronage :: PersistConnection -> PG.Connection -> ServerPartR
getAllMemberPatronage ref dbCn =
  path $ \(fiscalPeriodStr::String) -> do
    cpId <- getSessionCoopId ref
    let Just fiscalPeriod = decode $ LB.pack fiscalPeriodStr
    mpAll <- liftIO $ ptrngGetFor dbCn cpId fiscalPeriod 
    let (mp, mu) = M.partition MB.isJust mpAll
    okJSResp $ (mp, M.keys mu)


