{-# Language ScopedTypeVariables #-}
module Service
where

import Types
import Utils
import Persist
import Domain
import Serialize
import System.Log.Logger

import Happstack.Lite 
  (ok, path, dir, Response(..), ServerPart(..), ToMessage(..), lookBS, lookCookieValue,
   toResponseBS) 
import qualified Data.Maybe as MB
import qualified Data.List as L            
import Data.Time (fromGregorian , toGregorian, UTCTime(..), getCurrentTime,
                  addGregorianMonthsClip, addGregorianYearsClip)   
import Data.Map as M
import Data.Aeson (encode, decode)
import Data.Acid.Advanced   ( query', update' )
import Control.Monad.IO.Class (liftIO)  
import qualified Data.ByteString.Lazy.Char8 as LB 
import qualified Data.ByteString.Char8 as CB 

import qualified Database.HDBC.PostgreSQL as PG -- remove me

import Control.Monad(guard)

import System.Log.Logger as LG
import Text.Printf(printf)

import qualified Codec.Archive.Zip as ZP
import qualified System.Posix.Directory as DR
import qualified System.Posix.Files as FL
import qualified System.Process as SP

import Happstack.Lite
  (mkCookie, addCookies, expireCookie, lookCookieValue, CookieLife(Session))
import qualified Data.Time as CK
import System.Locale(defaultTimeLocale)
import Data.Acid.Advanced   ( query', update' )
import Control.Monad ( void )


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

getCooperative :: PersistConnection -> PG.Connection -> ServerPartR
getCooperative ref dbCn = do
  cpId <- getSessionCoopId ref
  (liftIO $ coopGet dbCn cpId) >>= okJSResp

putCooperative :: PersistConnection -> PG.Connection -> ServerPartR
putCooperative ref dbCn = do
  coop <- parseObject  
  cpId <- liftIO $ coopSave dbCn coop 
  coop <- liftIO $ coopGet dbCn cpId
  attachSession ref coop
  okJSResp ()

getCoopRegistrationState :: 
  PersistConnection -> PG.Connection -> ServerPart (Bool,Bool) 
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
  let lookRational = fmap readRational . lookString
  case method of 
    ProductiveHours -> do
      workw <- lookRational "workw"
      return 
        PatronageWeights{workw=workw,skillWeightedWorkw=0, 
     	        seniorityw = 0, qualityw = 0, 
		revenueGeneratedw = 0}
    Wages -> do
      skillWeightedWorkw <- lookRational "skillWeightedWorkw"
      return 
        PatronageWeights{workw=0,skillWeightedWorkw=skillWeightedWorkw, 
     	        seniorityw = 0, qualityw = 0, 
		revenueGeneratedw = 0}
    _ -> do
      workw <- lookRational "workw"
      skillWeightedWorkw <- lookRational "skillWeightedWorkw"
      case method of 
        SimpleMix -> 
          return 
            PatronageWeights{workw=workw,skillWeightedWorkw=skillWeightedWorkw, 
                             seniorityw = 0, qualityw = 0, 
                             revenueGeneratedw = 0}
        _ -> do
          seniorityw <- lookRational "seniorityw"
          return 
            PatronageWeights{workw=workw,skillWeightedWorkw=skillWeightedWorkw, 
                             seniorityw = seniorityw, qualityw = 0, 
                             revenueGeneratedw = 0}

lookPatronage :: AllocationMethod -> FiscalPeriod -> ServerPart WorkPatronage
lookPatronage method performedOver = 
  case method of 
    ProductiveHours -> do
      work <- lookRead "work"
      return WorkPatronage{work=work,skillWeightedWork=0,seniority=0,quality=0,
                           revenueGenerated=0,performedOver=performedOver}
    Wages -> do
      skillWeightedWork <- lookRead "skillWeightedWork"
      return WorkPatronage{work=0,skillWeightedWork=skillWeightedWork,seniority=0,
                           quality=0,revenueGenerated=0,performedOver=performedOver}
    _ -> do
      work <- lookRead "work"
      skillWeightedWork <- lookRead "skillWeightedWork"
      case method of 
        SimpleMix -> 
          return WorkPatronage{work=work,skillWeightedWork=skillWeightedWork,
                               seniority=0,quality=0,revenueGenerated=0,
                               performedOver=performedOver}
        _ -> do
          seniority <- lookRead "seniority"
          return WorkPatronage{work=work,skillWeightedWork=skillWeightedWork,
                               seniority=seniority,quality=0,revenueGenerated=0,
                               performedOver=performedOver}

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

putMember :: PersistConnection -> PG.Connection -> ServerPartR
putMember ref dbCn = do 
  cpId <- getSessionCoopId ref
  parseObject >>= (liftIO . mbrSave dbCn cpId) >>= okJSResp
     
getMember :: PersistConnection -> PG.Connection -> ServerPartR
getMember ref dbCn =
  path $ \(mid::Integer) -> do
    cpId <- getSessionCoopId ref
    (liftIO $ mbrGet dbCn cpId mid) >>= okJSResp

getMembers :: PersistConnection -> PG.Connection -> ServerPartR
getMembers ref dbCn = do 
  cpId <- getSessionCoopId ref
  today <- liftIO getCurrentDay
  (liftIO $ mbrGetAll dbCn cpId today) >>= okJSResp

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

postAllocateToMembers :: PersistConnection -> PG.Connection -> ServerPartR
postAllocateToMembers ref dbCn = 
  handleAllocateToMembers ref dbCn >>= okJSResp . snd

postScheduleAllocateDisbursal :: PersistConnection -> PG.Connection -> ServerPartR
postScheduleAllocateDisbursal ref dbCn = 
  do cpId <- getSessionCoopId ref
     allocateActionStr <- lookBS "allocateAction"
     let Just allocateAction = decode allocateActionStr
     disbursalSchedule <- liftIO $ dsbSchedGet dbCn cpId
     okJSResp $ scheduleDisbursalsFor allocateAction $ disbursalSchedule

handleAllocateToMembers 
  :: PersistConnection -> PG.Connection -> 
     ServerPart (FiscalPeriod, (M.Map Member MemberEquityAction))
handleAllocateToMembers ref dbCn =  
  do cpId <- getSessionCoopId ref
     today <- liftIO getCurrentDay
     overStr <- lookBS "over"
     let Just allocateOver = decode overStr
     Just res <- liftIO $ rsltGetForOver dbCn cpId allocateOver
     patronage <- liftIO $ ptrngGetFor dbCn cpId allocateOver     
     (name, parameters) <- liftIO $ allocStngGet dbCn cpId
     return $ 
       (allocateOver, 
        allocateEquityFor res (M.map MB.fromJust patronage) parameters today)

postAllocationDisbursal :: PersistConnection -> PG.Connection -> ServerPartR
postAllocationDisbursal ref dbCn = 
  do cpId <- getSessionCoopId ref
     (allocateOver, me) <- handleAllocateToMembers ref dbCn
     liftIO $ LG.infoM "Service.postAllocationDisbursal" $ 
       printf "%d. alloc for %s" cpId (show allocateOver)     
     disbursalSchedule <- liftIO $ dsbSchedGet dbCn cpId     
     liftIO $ mapM_
       (\(Member{memberId=mbrId}, allocAcn) -> do
         acnSaveToRolling dbCn cpId mbrId allocateOver allocAcn
         mapM_ 
           (acnSaveToRolling dbCn cpId mbrId allocateOver)
           (scheduleDisbursalsFor allocAcn disbursalSchedule))
       (M.toList me)
     today <- liftIO getCurrentDay
     (liftIO $ rsltUpdateAllocated dbCn cpId allocateOver today) >>= okJSResp
        

getActionsForMemberEquityAcct :: PersistConnection -> PG.Connection -> ServerPartR
getActionsForMemberEquityAcct ref dbCn = do 
  cpId <- getSessionCoopId ref
  mbrId <- lookRead "mbrId"
  acctId <- lookRead "acctId"
  all <- liftIO $ acnGetFor dbCn cpId mbrId acctId
  let (acns,rstOfs) = unzip all
  let acnBals = runningBalance acns
  okJSResp $ reverse $ zipWith (\(a,b) c -> (a,c,b)) acnBals rstOfs
  
--probably not necessary, just one mem at a time, merge with below
getAllMembersEquityAccounts :: PersistConnection -> PG.Connection -> ServerPartR
getAllMembersEquityAccounts ref dbCn = do 
  cpId <- getSessionCoopId ref
  (liftIO $ acctGetAll dbCn cpId) >>= okJSResp

getMemberEquityAccount :: PersistConnection -> PG.Connection -> ServerPartR
getMemberEquityAccount ref dbCn = do
  cpId <- getSessionCoopId ref
  mbrId <- lookRead "mbrId"
  acctId <- lookRead "acctId"
  (liftIO $ acctGet dbCn cpId mbrId acctId) >>= okJSResp

putEquityAction :: PersistConnection -> PG.Connection -> ServerPartR
putEquityAction ref dbCn = do
  cpId <- getSessionCoopId ref  
  mbrId <- lookRead "mbrId"
  acctId <- lookRead "acctId"
  actionType <- lookRead "actionType"
  amount <- lookRead "amount"
  performedOnStr <- lookString "performedOn"
  let Just performedOn = parseJSDate performedOnStr
  overStr <- lookBS "resultOf"
  let overVal = decode overStr
  let acn = MemberEquityAction{actionType=actionType,amount=amount,
  	       performedOn=performedOn}
  (liftIO $ acnSaveFor dbCn cpId mbrId acctId overVal acn) >>= okJSResp
 

exportAll :: PersistConnection -> PG.Connection -> ServerPartR
exportAll ref dbCn = do
  cpId <- getSessionCoopId ref
  UTCTime{utctDayTime=time1} <- liftIO getCurrentTime
  let time = show time1
  liftIO $ DR.createDirectory ("/tmp/" ++ time) FL.accessModes
  liftIO $ SP.system ("chmod 777 /tmp/" ++ time)
  liftIO $ acnExportFor dbCn cpId ("/tmp/" ++ time ++ "/capaActions.csv") 
  liftIO $ ptrngExportFor dbCn cpId ("/tmp/" ++ time ++ "/capaPatronage.csv")
  liftIO $ rsltExportFor dbCn cpId ("/tmp/" ++ time ++ "/capaResults.csv")
  liftIO $ stngExportFor dbCn cpId ("/tmp/" ++ time ++ "/capaSettings.csv")
  arch <- 
    liftIO $ ZP.addFilesToArchive [ZP.OptRecursive] ZP.emptyArchive [("/tmp/" ++ time)] 
  ok $ toResponseBS (CB.pack "application/zip") $ ZP.fromArchive arch

--authenticate util--
getSessionCoopId :: PersistConnection -> ServerPart Integer
getSessionCoopId ref = do 
  Globals{sessions=sessions} <- query' ref GetIt
  sessionId <- lookCookieValue "sessionId"
  let res = M.lookup sessionId sessions
  guard $ MB.isJust res
  let Just (_,cpId) = res
  return cpId

expireSession :: PersistConnection -> ServerPartR
expireSession ref = do
  g@Globals{sessions=sessions} <- query' ref GetIt
  sessionId <- lookCookieValue "sessionId"
  void $ update' ref $ PutIt g{sessions = M.delete sessionId sessions}
  expireCookie "sessionId"
  okJSResp ()

attachSession :: PersistConnection -> Cooperative -> ServerPart ()
attachSession ref Cooperative{cooperativeId=cpId, name=name, username=user} = do
  liftIO $ LG.infoM "Service.attachSession" $ printf "Resolved for %s, %s" user name
  utcNow <- liftIO CK.getCurrentTime
  let secs = CK.formatTime defaultTimeLocale "%s" utcNow
  let sessionId = secs
  addCookies [(Session, mkCookie "sessionId" sessionId)]
  g@Globals{sessions=sessions} <- query' ref GetIt
  void $ update' ref $ PutIt g{sessions = M.insert sessionId (user, cpId) sessions}
