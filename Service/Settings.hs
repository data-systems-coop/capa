module Service.Settings
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Data.Map as M

import Service.Security

authenticatedForRegister :: String -> String -> ServerPartR
authenticatedForRegister authUriBase registerUrl = do 
  retrieveProfile authUriBase >>= 
    redirect . (registerUrl ++) . urlEncode --url escape

getCoopRegistrationState :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) (Bool, Bool)
getCoopRegistrationState = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  liftIO $ coopRegisterState dbCn cpId 

putDefaultDisbursalSchedule :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
putDefaultDisbursalSchedule = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  defaultDisbSchedStr <- lift $ lookBS "disbursalSchedule"
  let Just defaultDisbSched = 
        (decode defaultDisbSchedStr)::Maybe DisbursalSchedule
  (liftIO $ dsbSchedSaveFor dbCn cpId defaultDisbSched) >>= (lift . okJSResp)

getDefaultDisbursalSchedule :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getDefaultDisbursalSchedule = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (liftIO $ dsbSchedGet dbCn cpId) >>= (lift . okJSResp)

getSeniorityMappings :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getSeniorityMappings = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (liftIO $ snrtyMpngsGet dbCn cpId) >>= (lift . okJSResp)

putCoopAllocateSettings :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
putCoopAllocateSettings = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    allocMethod <- lookRead "allocationMethod" 
    pw <- lookPatronageWeights allocMethod
    sm <- lookSeniorityMappings allocMethod
    (liftIO $ saveCoopAllocateSettings dbCn cpId allocMethod pw sm) >>= okJSResp

getAllocMethodDetail :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllocMethodDetail = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (method, wghts) <- (liftIO $ allocStngGet dbCn cpId) 
  lift $ okJSResp $ ((fieldDetails method), show method, wghts)

saveCoopAllocateSettings :: 
  Connection -> Integer -> AllocationMethod -> PatronageWeights -> 
    Maybe SeniorityMappings
    -> IO ()
saveCoopAllocateSettings dbCn cpId method patronageWeights seniorityMappings 
  | (method == ProductiveHours || method == Wages || method == SimpleMix) = 
    allocStngSaveFor dbCn cpId method patronageWeights
  | otherwise = do  
    allocStngSaveFor dbCn cpId method patronageWeights
    snrtyMpngsSaveFor dbCn cpId $ fromJust seniorityMappings
    

lookSeniorityMappings :: 
  AllocationMethod -> ServerPartT IO (Maybe SeniorityMappings)
lookSeniorityMappings method 
  | (method == ProductiveHours || method == Wages || method == SimpleMix) = 
      return Nothing
  | otherwise = do 
      snrtyLvlsStr <- lookBS "seniorityLevels"
      let Just snrtyLvls = 
            (decode snrtyLvlsStr)::Maybe [(SeniorityMappingEntry,SeniorityLevel)]
      return $ Just $ M.fromList $ snrtyLvls
      
allocMethods :: [AllocationMethod]
allocMethods = [ProductiveHours, Wages, SimpleMix, SeniorityMix]--, ElaborateMix]

fieldDetails :: AllocationMethod -> [PatronageFieldDetail]
fieldDetails ProductiveHours =  [workFieldDetail]
fieldDetails Wages = [skillWeightedWorkFieldDetail]
fieldDetails SimpleMix = [workFieldDetail, skillWeightedWorkFieldDetail]
fieldDetails SeniorityMix = 
  [workFieldDetail, skillWeightedWorkFieldDetail, seniorityFieldDetail]
fieldDetails ElaborateMix = 
  [workFieldDetail, skillWeightedWorkFieldDetail, seniorityFieldDetail,
   qualityFieldDetail, revenueGeneratedFieldDetail]

lookPatronageWeights :: AllocationMethod -> ServerPartT IO PatronageWeights
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
