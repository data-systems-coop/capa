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
import Control.Monad.IO.Class (liftIO)  -- debug
import qualified Data.ByteString.Lazy.Char8 as LB 
import qualified Data.ByteString.Char8 as CB 

import qualified Database.HDBC.PostgreSQL as PG -- remove me

import Control.Monad(guard)

import System.Log.Logger as LG
import Text.Printf(printf)

import qualified Codec.Archive.Zip as ZP

-- MEDIUM
-- for provided year, provide 2 years back and forward
getLatestFiscalPeriods :: PersistConnection -> PG.Connection -> ServerPartR
getLatestFiscalPeriods ref dbCn = do
  cpId <- getSessionCoopId ref
  Cooperative{fiscalCalendarType=ft} <- liftIO $ coopGet dbCn cpId
  let FiscalCalendarType{startf=startMonth, periodTypef=pt} = ft
  UTCTime{utctDay=day,utctDayTime=_} <- liftIO getCurrentTime
  let (endYear,_,_) = toGregorian $ addGregorianYearsClip 2 day
  let end = fromGregorian endYear startMonth 1
  let stepBack = 
        if pt == Year 
        then addGregorianYearsClip (-1)
        else addGregorianMonthsClip (-3)
  let enumStarts d = d : (enumStarts $ stepBack d)
  let periods = 
        fmap 
          ((\(yr,mo,_) -> FiscalPeriod (GregorianMonth yr mo) pt) . toGregorian)
          (take 10 $ enumStarts end)
  ok $ toResponse $ JSONData periods

getCooperative :: PersistConnection -> PG.Connection -> ServerPartR
getCooperative ref dbCn = do
  cpId <- getSessionCoopId ref
  (liftIO $ coopGet dbCn cpId) >>= 
    ok . toResponse . JSONData 

-- EASY 
-- putCooperative :: PersistentConnection -> ServerPartR

-- getDefaultDisbursalSchedule (not rush)

putDefaultDisbursalSchedule :: PersistConnection -> PG.Connection -> ServerPartR
putDefaultDisbursalSchedule ref dbCn = do 
  cpId <- getSessionCoopId ref
  defaultDisbSchedStr <- lookBS "disbursalSchedule"
  let Just defaultDisbSched = 
        (decode defaultDisbSchedStr)::Maybe DisbursalSchedule
  liftIO $ dsbSchedSaveFor dbCn cpId defaultDisbSched
  ok $ toResponse ()

getSeniorityMappings :: PersistConnection -> PG.Connection -> ServerPartR
getSeniorityMappings ref dbCn = do
  cpId <- getSessionCoopId ref
  (liftIO $ snrtyMpngsGet dbCn cpId) >>= 
    ok . toResponse . JSONData . M.toList

putCoopAllocateSettings :: PersistConnection -> PG.Connection -> ServerPartR
putCoopAllocateSettings ref dbCn = do 
      cpId <- getSessionCoopId ref
      allocMethod <- lookRead "allocationMethod" 
      pw <- lookPatronageWeights allocMethod
      sm <- lookSeniorityMappings allocMethod
      liftIO $ saveCoopAllocateSettings dbCn cpId allocMethod pw sm
      ok $ toResponse ()

getAllocMethodDetail :: PersistConnection -> PG.Connection -> ServerPartR
getAllocMethodDetail ref dbCn = do
  cpId <- getSessionCoopId ref
  (method, _) <- liftIO $ allocStngGet dbCn cpId 
  let fieldDetails = 
       case method of 
        ProductiveHours -> [workFieldDetail]
        Wages -> [skillWeightedWorkFieldDetail]
        SimpleMix -> [workFieldDetail, skillWeightedWorkFieldDetail]
        SeniorityMix -> 
          [workFieldDetail, skillWeightedWorkFieldDetail, seniorityFieldDetail]
        ElaborateMix -> 
          [workFieldDetail, skillWeightedWorkFieldDetail, seniorityFieldDetail,
           qualityFieldDetail, revenueGeneratedFieldDetail]
  ok $ toResponse $ JSONData fieldDetails
      
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
      skillWeightedWorkw <- lookRead "skillWeightedWorkw"
      return 
        PatronageWeights{workw=0,skillWeightedWorkw=skillWeightedWorkw, 
     	        seniorityw = 0, qualityw = 0, 
		revenueGeneratedw = 0}
    _ -> do
      workw <- lookRational "workw"
      skillWeightedWorkw <- lookRead "skillWeightedWorkw"
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
          (liftIO . ptrngSaveFor dbCn cpId idIn)
     	ok $ toResponse ()

getAllMemberPatronage :: PersistConnection -> PG.Connection -> ServerPartR
getAllMemberPatronage ref dbCn =
  path $ \(fiscalPeriodStr::String) -> do
    cpId <- getSessionCoopId ref
    let Just fiscalPeriod = decode $ LB.pack fiscalPeriodStr
    mpAll <- liftIO $ ptrngGetFor dbCn cpId fiscalPeriod 
    let (mp, mu) = M.partition MB.isJust mpAll
    ok $ toResponse $ JSONData $ (M.toList mp, M.keys mu)

-- EASY
putMember :: PersistConnection -> ServerPartR
putMember ref = do -- get all parameters for member
  cpId <- getSessionCoopId ref
  firstName <- lookString "firstName"
  let member = Member firstName 1
  -- g <- query' ref GetIt
  -- let mems = members g
  -- g2 <- update' ref (PutIt g{members = mems ++ [member]}) -- ignore
  ok $ toResponse ()
     
getMember :: PersistConnection -> PG.Connection -> ServerPartR
getMember ref dbCn = do 
  path $ \(mid::Integer) -> do
    cpId <- getSessionCoopId ref
    (liftIO $ mbrGet dbCn cpId mid) >>= (ok . toResponse . JSONData)

getMembers :: PersistConnection -> PG.Connection -> ServerPartR
getMembers ref dbCn = do 
  cpId <- getSessionCoopId ref
  (liftIO $ mbrGetAll dbCn cpId) >>= 
    ok . toResponse . JSONData

getAllFinancialResultsDetail :: PersistConnection -> PG.Connection -> ServerPartR
getAllFinancialResultsDetail ref dbCn = do 
  cpId <- getSessionCoopId ref
  res <- liftIO $ rsltGetAll dbCn cpId
  ok $ toResponse $ JSONData res
       
putFinancialResults :: PersistConnection -> PG.Connection -> ServerPartR
putFinancialResults ref dbCn = do 
  cpId <- getSessionCoopId ref
  surplus <- lookRead "surplus"
  overStr <- lookBS "over"
  let Just over = decode overStr
  let res = FinancialResults over surplus Nothing
  liftIO $ rsltSaveFor dbCn cpId res
  ok $ toResponse ()

postAllocateToMembers :: PersistConnection -> PG.Connection -> ServerPartR
postAllocateToMembers ref dbCn = 
  handleAllocateToMembers ref dbCn
  >>= ok . toResponse . JSONData . M.toList . snd

postScheduleAllocateDisbursal :: PersistConnection -> PG.Connection -> ServerPartR
postScheduleAllocateDisbursal ref dbCn = 
  do cpId <- getSessionCoopId ref
     allocateActionStr <- lookBS "allocateAction"
     let Just allocateAction = decode allocateActionStr
     disbursalSchedule <- liftIO $ dsbSchedGet dbCn cpId
     ok $ toResponse $ 
       JSONData $ scheduleDisbursalsFor allocateAction $ disbursalSchedule

handleAllocateToMembers 
  :: PersistConnection -> PG.Connection -> 
     ServerPart (FiscalPeriod, (M.Map Member MemberEquityAction))
handleAllocateToMembers ref dbCn =  
  do cpId <- getSessionCoopId ref
     UTCTime{utctDay=day,utctDayTime=_} <- liftIO getCurrentTime
     overStr <- lookBS "over"
     let Just allocateOver = decode overStr
     Just res <- liftIO $ rsltGetForOver dbCn cpId allocateOver
     patronage <- liftIO $ ptrngGetFor dbCn cpId allocateOver     
     (name, parameters) <- liftIO $ allocStngGet dbCn cpId
     return $ 
       (allocateOver, 
        allocateEquityFor res (M.map MB.fromJust patronage) parameters day)

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
     UTCTime{utctDay=day,utctDayTime=_} <- liftIO getCurrentTime
     liftIO $ rsltUpdateAllocated dbCn cpId allocateOver day
     ok $ toResponse ()
        

getActionsForMemberEquityAcct :: PersistConnection -> PG.Connection -> ServerPartR
getActionsForMemberEquityAcct ref dbCn = do 
  cpId <- getSessionCoopId ref
  mbrId <- lookRead "mbrId"
  acctId <- lookRead "acctId"
  all <- liftIO $ acnGetFor dbCn cpId mbrId acctId
  let (acns,rstOfs) = unzip all
  let acnBals = runningBalance acns
  ok $ toResponse $ JSONData $ reverse $ zipWith (\(a,b) c -> (a,c,b)) acnBals rstOfs
  
--probably not necessary, just one mem at a time, merge with below
getAllMembersEquityAccounts :: PersistConnection -> PG.Connection -> ServerPartR
getAllMembersEquityAccounts ref dbCn = do 
  cpId <- getSessionCoopId ref
  (liftIO $ acctGetAll dbCn cpId) >>=
    ok . toResponse . JSONData . M.toList 

getMemberEquityAccount :: PersistConnection -> PG.Connection -> ServerPartR
getMemberEquityAccount ref dbCn = do
  cpId <- getSessionCoopId ref
  mbrId <- lookRead "mbrId"
  acctId <- lookRead "acctId"
  (liftIO $ acctGet dbCn cpId mbrId acctId) >>=
    ok . toResponse . JSONData

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
  liftIO $ acnSaveFor dbCn cpId mbrId acctId overVal acn 
  ok $ toResponse ()

exportAll :: PersistConnection -> PG.Connection -> ServerPartR
exportAll ref dbCn = do
  cpId <- getSessionCoopId ref
  -- make dir with id + add universal permisions
  liftIO $ acnExportFor dbCn cpId "/tmp/101/capaActions.csv" -- capaActions-.csv
  liftIO $ ptrngExportFor dbCn cpId "/tmp/101/capaPatronage.csv" -- capaActions-.csv
  -- res
  -- settings + list -> string for disbursals
  arch <- liftIO $ ZP.addFilesToArchive [ZP.OptRecursive] ZP.emptyArchive ["/tmp/101"] 
  -- delete directory
  -- ZP.fromArchive arch 
  -- stream zip fiel back??
  ok $ toResponseBS (CB.pack "application/zip") $ ZP.fromArchive arch

--util--
getSessionCoopId :: PersistConnection -> ServerPart Integer
getSessionCoopId ref = do 
  Globals{sessions=sessions} <- query' ref GetIt
  sessionId <- lookCookieValue "sessionId"
  let res = M.lookup sessionId sessions
  guard $ MB.isJust res
  let Just (_,cpId) = res
  return cpId
