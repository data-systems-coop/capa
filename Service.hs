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
  (ok, path, dir, Response(..), ServerPart(..), ToMessage(..), lookBS, lookCookieValue) 
import qualified Data.Maybe as MB
import qualified Data.List as L            
import Data.Time (fromGregorian , toGregorian, UTCTime(..), getCurrentTime,
                  addGregorianMonthsClip, addGregorianYearsClip)   
import Data.Map as M
import Data.Aeson (encode, decode)
import Data.Acid.Advanced   ( query', update' )
import Control.Monad.IO.Class (liftIO)  -- debug
import qualified Data.ByteString.Lazy.Char8 as LB 

import qualified Database.HDBC.PostgreSQL as PG -- remove me

import Control.Monad(guard)

import System.Log.Logger as LG
import Text.Printf(printf)

-- MEDIUM
-- for provided year, provide 2 years back and forward
getLatestFiscalPeriods :: PersistConnection -> ServerPartR
getLatestFiscalPeriods ref = do
  cpId <- getSessionCoopId ref
  Globals{cooperative=Cooperative{fiscalCalendarType=ft}} <- query' ref GetIt
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

-- EASY 
-- getCooperative :: PersistentConnection -> ServerPartR

-- putCooperative :: PersistentConnection -> ServerPartR

-- getDefaultDisbursalSchedule (not rush)

putDefaultDisbursalSchedule :: PersistConnection -> PG.Connection -> ServerPartR
putDefaultDisbursalSchedule ref dbCn = do 
  cpId <- getSessionCoopId ref
  defaultDisbSchedStr <- lookBS "defaultDisbursalSchedule"
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

-- EASY - detail
getMembers :: PersistConnection -> PG.Connection -> ServerPartR
getMembers ref dbCn = do -- get sum of equity balances with each member
  cpId <- getSessionCoopId ref
  ms <- liftIO $ mbrGetAll dbCn cpId
  ok $ toResponse $ JSONData ms

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
        

-- EASY --
-- getActionsForMemberEquityAccount
    -- pair each result with running balance, and any associated allocation entry
-- getMemberEquityAccounts        

-- EASY 
putEquityAction :: PersistConnection -> ServerPartR
putEquityAction ref = do
  cpId <- getSessionCoopId ref  
  --member? --acctId?
  actionType <- lookRead "actionType"
  amount <- lookRead "amount"
  performedOnStr <- lookBS "performedOn"
  let Just performedOn = decode performedOnStr
  let act = MemberEquityAction{actionType=actionType,amount=amount,
  	       performedOn=performedOn}
  -- db save here
  ok $ toResponse ()


--util 

getSessionCoopId :: PersistConnection -> ServerPart Integer
getSessionCoopId ref = do 
  Globals{sessions=sessions} <- query' ref GetIt
  sessionId <- lookCookieValue "sessionId"
  let res = M.lookup sessionId sessions
  guard $ MB.isJust res
  let Just (_,cpId) = res
  return cpId
  