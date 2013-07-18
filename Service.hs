{-# Language ScopedTypeVariables #-}
module Service
where

import Types
import Utils
import Persist
import Domain
import Serialize

import Happstack.Lite 
  (ok, path, dir, Response(..), ServerPart(..), ToMessage(..), lookBS) 
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

type ServerPartR = ServerPart Response

-- for provided year, provide 2 years back and forward
getLatestFiscalPeriods :: PersistConnection -> ServerPartR
getLatestFiscalPeriods ref = do
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

-- putCooperative :: PersistentConnection -> ServerPartR
-- getCooperative :: PersistentConnection -> ServerPartR

-- putDefaultDisbursalSchedule
-- getDefaultDisbursalSchedule

-- getCalcMethod
putCoopAllocateSettings :: PersistConnection -> ServerPartR
putCoopAllocateSettings ref = do 
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



putMember :: PersistConnection -> ServerPartR
putMember ref = do -- get all parameters for member
  firstName <- lookString "firstName"
  let member = Member firstName 1
  g <- query' ref GetIt
  let mems = members g
  g2 <- update' ref (PutIt g{members = mems ++ [member]}) -- ignore
  ok $ toResponse ()
     
getMember :: PersistConnection -> ServerPartR
getMember ref = do 
  path $ \(mid::Integer) -> do
    Globals{members=mems} <- query' ref GetIt  -- ignore
    let possibleMem = L.find ((== mid) . memberId) mems
    ok $ toResponse $ JSONData possibleMem

-- detail
getMembers :: PersistConnection -> PG.Connection -> ServerPartR
getMembers ref dbCn = do -- get sum of equity balances with each member
  ms <- liftIO $ mbrGetFor dbCn 1
  ok $ toResponse $ JSONData ms

-- putMemberRquityAccount


-- getMemberEquityAccounts
        


getAllMemberPatronage :: PersistConnection -> PG.Connection -> ServerPartR
getAllMemberPatronage ref dbCn =
  path $ \(fiscalPeriodStr::String) -> do
    let Just fiscalPeriod = decode $ LB.pack fiscalPeriodStr
    mpAll <- liftIO $ ptrngGetFor dbCn 0 fiscalPeriod 
    let (mp, mu) = M.partition MB.isJust mpAll
    ok $ toResponse $ JSONData $ (M.toList mp, M.keys mu)


-- handle fields based on alloc method
putMemberPatronage :: PersistConnection -> PG.Connection -> ServerPartR
putMemberPatronage ref dbCn = 
  path $ \(idIn::Integer) -> dir "patronage" $ path $ \(performedOverStr::String) ->
     do work <- lookRead "work"
     	skillWeightedWork <- lookRead "skillWeightedWork"
        seniority <- lookRead "seniority"
     	-- quality <- lookRead "quality"
     	-- revenueGenerated <- lookRead "revenueGenerated"
	let Just performedOver = decode $ LB.pack performedOverStr
     	let p = WorkPatronage{work=work, 
	      	       skillWeightedWork=skillWeightedWork,
	 	       seniority=seniority, quality=0,
		       revenueGenerated=0,performedOver=performedOver}
        liftIO $ ptrngSaveFor dbCn 1 idIn p
     	ok $ toResponse ()

getAllFinancialResultsDetail :: PersistConnection -> PG.Connection -> ServerPartR
getAllFinancialResultsDetail ref dbCn = do 
  res <- liftIO $ rsltGetFor dbCn 0 
  ok $ toResponse $ JSONData res
       
putFinancialResults :: PersistConnection -> PG.Connection -> ServerPartR
putFinancialResults ref dbCn = do 
  surplus <- lookRead "surplus"
  overStr <- lookBS "over"
  let Just over = decode overStr
  let res = FinancialResults over surplus Nothing
  liftIO $ rsltSaveFor dbCn 1 res
  ok $ toResponse ()
     
-- obsolete
postAllocateToMembers :: PersistConnection -> ServerPartR
postAllocateToMembers ref = 
  do UTCTime{utctDay=day,utctDayTime=_} <- liftIO getCurrentTime
     overStr <- lookBS "over"
     let Just allocateOver = decode overStr
     Globals{financialResults=fr, settings=settings, patronage=patronage} <- 
       query' ref GetIt
     let Just res = L.find ((== allocateOver) . over) fr
     let Just (name, parameters, _) = settings
     -- let memPatr = M.map  -- retrieve for over period
     let me = allocateEquityFor res (M.map head patronage) parameters day
     ok $ toResponse $ JSONData $ M.toList me

postAllocationDisbursal :: PersistConnection -> PG.Connection -> ServerPartR
postAllocationDisbursal ref dbCn = 
  do UTCTime{utctDay=day,utctDayTime=_} <- liftIO getCurrentTime
     overStr <- lookBS "allocateOver"
     let Just allocateOver = decode overStr
     g@Globals{financialResults=fr, patronage=patronage, 
               allocations=allocs, accounts=accounts} <- 
       query' ref GetIt
     (name, parameters) <- liftIO $ allocStngGetFor dbCn 1
     disbursalSchedule <- liftIO $ dsbSchedGetFor dbCn 1
     let (bef,res:aft) = L.break ((== allocateOver) . over) fr
     -- let memPatr = M.map  ***** -- retrieve for over period
     let me = allocateEquityFor res (M.map head patronage) parameters day
     -- liftIO $ putStrLn $ show me
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
         in mapM_ (\acn -> acnSaveFor dbCn 1 mbrId acctId allocateOver acn) acns)
       (toList accounts2)
     let res2 = res{allocatedOn=Just day}          
     -- liftIO $ putStrLn $ show (accounts2, allActions)
     let fr2 = bef ++ (res2 : aft)
     let allocs2 = M.insert res2 allActions (M.delete res allocs)
     -- _ <- update' ref (PutIt g{financialResults = fr2, allocations = allocs2,
     --                        accounts = accounts2})
     ok $ toResponse ()
        

-- getActionsForMemberEquityAccount
    -- pair each result with running balance, and any associated allocation entry
        
putEquityAction :: PersistConnection -> ServerPartR
putEquityAction ref = 
  do actionType <- lookRead "actionType"
     amount <- lookRead "amount"
     performedOnStr <- lookBS "performedOn"
     let Just performedOn = decode performedOnStr
     let act = MemberEquityAction{actionType=actionType,amount=amount,
     	          performedOn=performedOn}
     ok $ toResponse ()

postScheduleAllocateDisbursal :: PersistConnection -> PG.Connection -> ServerPartR
postScheduleAllocateDisbursal ref dbCn = 
  do allocateActionStr <- lookBS "allocateAction"
     let Just allocateAction = decode allocateActionStr
     disbursalSchedule <- liftIO $ dsbSchedGetFor dbCn 1
     ok $ toResponse $ 
       JSONData $ scheduleDisbursalsFor allocateAction $ disbursalSchedule
                          

