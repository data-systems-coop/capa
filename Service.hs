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


type ServerPartR = ServerPart Response

coopSummary :: PersistConnection -> ServerPartR
coopSummary ref = do 
  v <- query' ref GetIt
  ok $ toResponse $ show v

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
putCalcMethod :: PersistConnection -> ServerPartR
putCalcMethod ref = 
  path $ \(methodName::String) -> do 
      let lookRational = fmap readRational . lookString
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
      g <- query' ref GetIt
      g2 <- update' ref $ PutIt g{settings = Just (methodName, pw, [])}
      ok $ toResponse ()



putMember :: PersistConnection -> ServerPartR
putMember ref = do -- get all parameters for member
  firstName <- lookString "firstName"
  let member = Member firstName 1
  g <- query' ref GetIt
  let mems = members g
  g2 <- update' ref (PutIt g{members = mems ++ [member]})
  ok $ toResponse ()
     
getMember :: PersistConnection -> ServerPartR
getMember ref = do 
  path $ \(mid::Integer) -> do
    Globals{members=mems} <- query' ref GetIt
    let possibleMem = L.find ((== mid) . memberId) mems
    ok $ toResponse $ JSONData possibleMem

-- detail
getMembers :: PersistConnection -> ServerPartR
getMembers ref = do -- get sum of equity balances with each member
  g <- query' ref GetIt
  let ms = members g
  ok $ toResponse $ JSONData ms

-- putMemberRquityAccount


-- getMemberEquityAccounts
        


getAllMemberPatronage :: PersistConnection -> ServerPartR
getAllMemberPatronage ref = 
  path $ \(fiscalPeriodStr::String) -> do
    let Just fiscalPeriod = decode $ LB.pack fiscalPeriodStr
    Globals{patronage=mps} <- query' ref GetIt 
    let mpAll = M.map (L.find (\p -> performedOver p == fiscalPeriod)) mps 
    let (mp, mu) = M.partition MB.isJust mpAll
    ok $ toResponse $ JSONData $ (M.toList mp, M.keys mu)


putMemberPatronage :: PersistConnection -> ServerPartR
putMemberPatronage ref = 
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
        g <- query' ref GetIt
     	let Just m = L.find ((\i -> i == idIn) . memberId) $ members g
     	g2 <- update' ref (PutIt g{patronage = M.insert m [p] $ patronage g})
     	ok $ toResponse ()

getAllFinancialResultsDetail :: PersistConnection -> ServerPartR
getAllFinancialResultsDetail ref = do 
  g <- query' ref GetIt
  let res = financialResults g
  ok $ toResponse $ JSONData res
       
putFinancialResults :: PersistConnection -> ServerPartR
putFinancialResults ref = 
  do surplus <- lookRead "surplus"
     overStr <- lookBS "over"
     let Just over = decode overStr
     let res = FinancialResults over surplus Nothing
     g <- query' ref GetIt
     let allRes = financialResults g
     _ <- update' ref (PutIt g{financialResults=allRes ++ [res]})
     ok $ toResponse ()
     

postAllocateToMembers :: PersistConnection -> ServerPartR
postAllocateToMembers ref = 
  do UTCTime{utctDay=day,utctDayTime=_} <- liftIO getCurrentTime
     overStr <- lookBS "over"
     let Just allocateOver = decode overStr
     Globals{financialResults=fr, settings=settings, patronage=patronage} <- 
       query' ref GetIt
     let Just res = L.find ((== allocateOver) . over) fr
     let Just (name, parameters, _) = settings
     -- let memPatr = M.map  *****
     let me = allocateEquityFor res (M.map head patronage) parameters day
     ok $ toResponse $ JSONData $ M.toList me

-- postAllocationDisbursal
    -- updated fiscal period entry, all allocs, all distribs    
    -- let res = FinancialResults over surplus $ Just day
        

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

postScheduleAllocateDisbursal :: PersistConnection -> ServerPartR
postScheduleAllocateDisbursal ref = 
  do allocateActionStr <- lookBS "allocateAction"
     let Just allocateAction = decode allocateActionStr
     g <- query' ref GetIt
     let Just (_, _, disbursalSchedule) = settings g
     ok $ toResponse $ show $ scheduleDisbursalsFor allocateAction $ disbursalSchedule
                          

