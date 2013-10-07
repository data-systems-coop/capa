module Service.Settings
where

import Types
import Utils
import Persist
import Domain
import Serialize

import Happstack.Lite (path, dir, ServerPart(..), lookBS) 
import Happstack.Server (look)
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

getCoopRegistrationState :: PersistConnection -> PG.Connection -> ServerPart (Bool,Bool)
getCoopRegistrationState ref dbCn = do
  cpId <- getSessionCoopId ref
  liftIO $ coopRegisterState dbCn cpId 

putDefaultDisbursalSchedule :: PersistConnection -> PG.Connection -> ServerPartR
putDefaultDisbursalSchedule ref dbCn = do 
  cpId <- getSessionCoopId ref
  defaultDisbSchedStr <- lookBS "disbursalSchedule"
  let Just defaultDisbSched = 
        (decode defaultDisbSchedStr)::Maybe DisbursalSchedule
  (liftIO $ dsbSchedSaveFor dbCn cpId defaultDisbSched) >>= okJSResp

getDefaultDisbursalSchedule :: PersistConnection -> PG.Connection -> ServerPartR
getDefaultDisbursalSchedule ref dbCn = do 
  cpId <- getSessionCoopId ref 
  (liftIO $ dsbSchedGet dbCn cpId) >>= okJSResp

getSeniorityMappings :: PersistConnection -> PG.Connection -> ServerPartR
getSeniorityMappings ref dbCn = do
  cpId <- getSessionCoopId ref
  (liftIO $ snrtyMpngsGet dbCn cpId) >>= okJSResp

putCoopAllocateSettings :: PersistConnection -> PG.Connection -> ServerPartR
putCoopAllocateSettings ref dbCn = do 
      cpId <- getSessionCoopId ref
      allocMethod <- lookRead "allocationMethod" 
      pw <- lookPatronageWeights allocMethod
      sm <- lookSeniorityMappings allocMethod
      (liftIO $ saveCoopAllocateSettings dbCn cpId allocMethod pw sm) >>= okJSResp

getAllocMethodDetail :: PersistConnection -> PG.Connection -> ServerPartR
getAllocMethodDetail ref dbCn = do
  cpId <- getSessionCoopId ref
  (method, wghts) <- (liftIO $ allocStngGet dbCn cpId) 
  okJSResp $ ((fieldDetails method), show method, wghts)

saveCoopAllocateSettings :: 
  PG.Connection -> Integer -> AllocationMethod -> PatronageWeights -> 
    Maybe SeniorityMappings
    -> IO ()
saveCoopAllocateSettings dbCn cpId method patronageWeights seniorityMappings 
  | (method == ProductiveHours || method == Wages || method == SimpleMix) = 
    allocStngSaveFor dbCn cpId method patronageWeights
  | otherwise = do  
    allocStngSaveFor dbCn cpId method patronageWeights
    snrtyMpngsSaveFor dbCn cpId $ MB.fromJust seniorityMappings
    

lookSeniorityMappings :: 
  AllocationMethod -> ServerPart (Maybe SeniorityMappings)
lookSeniorityMappings method 
  | (method == ProductiveHours || method == Wages || method == SimpleMix) = 
      return Nothing
  | otherwise = do 
      snrtyLvlsStr <- lookBS "seniorityLevels"
      let Just snrtyLvls = (decode snrtyLvlsStr)::Maybe [SeniorityMappingEntry]
      return $ Just $ M.fromList $ zip snrtyLvls [1 .. toInteger (length snrtyLvls)]
      
allocMethods :: [AllocationMethod]
allocMethods = [ProductiveHours, Wages, SimpleMix] --, SeniorityMix], ElaborateMix]

fieldDetails :: AllocationMethod -> [PatronageFieldDetail]
fieldDetails ProductiveHours =  [workFieldDetail]
fieldDetails Wages = [skillWeightedWorkFieldDetail]
fieldDetails SimpleMix = [workFieldDetail, skillWeightedWorkFieldDetail]
fieldDetails SeniorityMix = 
  [workFieldDetail, skillWeightedWorkFieldDetail, seniorityFieldDetail]
fieldDetails ElaborateMix = 
  [workFieldDetail, skillWeightedWorkFieldDetail, seniorityFieldDetail,
   qualityFieldDetail, revenueGeneratedFieldDetail]

lookPatronageWeights :: AllocationMethod -> ServerPart PatronageWeights
lookPatronageWeights method = do
  let lookRational = fmap readRational . look
  case method of 
    ProductiveHours -> do
      workw <- lookRational "workw"
      return def{workw=workw}
    Wages -> do
      skillWeightedWorkw <- lookRational "skillWeightedWorkw"
      return def{workw=0,skillWeightedWorkw=skillWeightedWorkw}
    _ -> do
      workw <- lookRational "workw"
      skillWeightedWorkw <- lookRational "skillWeightedWorkw"
      case method of 
        SimpleMix -> 
          return 
            def{workw=workw,skillWeightedWorkw=skillWeightedWorkw}
        _ -> do
          seniorityw <- lookRational "seniorityw"
          return def{workw=workw,skillWeightedWorkw=skillWeightedWorkw,
                     seniorityw = seniorityw}
