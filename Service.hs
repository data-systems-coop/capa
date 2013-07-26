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

-- getDefaultDisbursalSchedule
-- putDefaultDisbursalSchedule

-- HARDER
-- getCalcMethod
putCoopAllocateSettings :: PersistConnection -> ServerPartR
putCoopAllocateSettings ref = do 
      cpId <- getSessionCoopId ref
      let lookRational = fmap readRational . lookString
      allocMethod <- lookRead "allocationMethod" 
      workw <- lookRational "workw"
      skillWeightedWorkw <- lookRational "skillWeightedWorkw"
      seniorityw <- lookRational "seniorityw"
      qualityw <- lookRational "qualityw"
      revenueGeneratedw <- lookRational "revenueGeneratedw"
      let pw = PatronageWeights{workw=workw, 
     	      	  skillWeightedWorkw=skillWeightedWorkw, 
     	          seniorityw = seniorityw, 
		  qualityw = qualityw, 
		  revenueGeneratedw = revenueGeneratedw}
      -- read seniority levels
      g@Globals{settings=Just(_,_,disb)} <- query' ref GetIt
      g2 <- update' ref $ PutIt g{settings = Just (allocMethod, pw, disb)}
      ok $ toResponse ()

-- handle fields based on alloc method
putMemberPatronage :: PersistConnection -> PG.Connection -> ServerPartR
putMemberPatronage ref dbCn = 
  path $ \(idIn::Integer) -> dir "patronage" $ path $ \(performedOverStr::String) -> do
        cpId <- getSessionCoopId ref
        work <- lookRead "work"
     	skillWeightedWork <- lookRead "skillWeightedWork"
        seniority <- lookRead "seniority"
     	-- quality <- lookRead "quality"
     	-- revenueGenerated <- lookRead "revenueGenerated"
	let Just performedOver = decode $ LB.pack performedOverStr
     	let p = WorkPatronage{work=work, 
	      	       skillWeightedWork=skillWeightedWork,
	 	       seniority=seniority, quality=0,
		       revenueGenerated=0,performedOver=performedOver}
        liftIO $ ptrngSaveFor dbCn cpId idIn p
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
     
-- EASY
getMember :: PersistConnection -> ServerPartR
getMember ref = do 
  path $ \(mid::Integer) -> do
    cpId <- getSessionCoopId ref
    -- Globals{members=mems} <- query' ref GetIt  -- ignore
    let mems = []
    let possibleMem = L.find ((== mid) . memberId) mems
    ok $ toResponse $ JSONData possibleMem

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

-- MEDIUM 
postAllocationDisbursal :: PersistConnection -> PG.Connection -> ServerPartR
postAllocationDisbursal ref dbCn = 
  do cpId <- getSessionCoopId ref
     (allocateOver, me) <- handleAllocateToMembers ref dbCn
     liftIO $ LG.infoM "Service.postAllocationDisbursal" $ 
       printf "%s. alloc for %s" cpId (show allocateOver)
     -- for each member/alloc
     --   for each disbursed
     --     acnSaveToRolling db cp mbr allocover acn
     -- update financialRes such that allocOn = Just Day

     g@Globals{accounts=accounts} <- 
       query' ref GetIt
     disbursalSchedule <- liftIO $ dsbSchedGet dbCn cpId
     let (accounts2, allActions) = 
           M.foldlWithKey 
                 (\(accounts, allActions) mem allocateAction -> 
                   let disb = scheduleDisbursalsFor allocateAction $ disbursalSchedule
                       memActions = allocateAction : disb
                       -- use the rolling account 
                       (account, _) = head $ toList $ accounts ! mem 
                       accounts2 = 
                         M.update 
                           (\mp -> 
                             Just $ 
                             M.update (\a -> Just (a ++ memActions)) account mp)
                           mem 
                           accounts 
                   in (accounts2 , allActions ++ memActions))
                 (accounts, [])
                 me
     liftIO $ mapM_
       (\(Member{memberId=mbrId}, accts) -> 
         let (MemberEquityAccount{ida=acctId},acns) = head $ toList $ accts
         in mapM_ (\acn -> acnSaveFor dbCn cpId mbrId acctId allocateOver acn) acns)
       (toList accounts2)
     UTCTime{utctDay=day,utctDayTime=_} <- liftIO getCurrentTime
     ok $ toResponse ()
        

-- EASY --
-- getActionsForMemberEquityAccount
    -- pair each result with running balance, and any associated allocation entry
-- getMemberEquityAccounts        

-- EASY 
putEquityAction :: PersistConnection -> ServerPartR
putEquityAction ref = do
  cpId <- getSessionCoopId ref  
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
  