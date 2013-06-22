{-# Language OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, 
    	     TemplateHaskell, TypeFamilies #-}
module Main (main) where

import Happstack.Lite

import Happstack.Server.Routing as HSR (dirs)

import Text.Blaze.Html5 (html, p, toHtml)
import Blaze.ByteString.Builder (toByteString)
import Heist (loadTemplates, HeistConfig(..), HeistState, initHeist,
              defaultLoadTimeSplices, defaultInterpretedSplices)
import Heist.Interpreted (renderTemplate)
import Control.Monad.Trans.Either
import Control.Monad.Identity

import Data.Text.Lazy (unpack)
import qualified Data.Text as DT
import qualified Data.ByteString.Char8 as B

import Data.Monoid

import Data.Ratio ((%))
--import Data.Default 

import qualified Data.Map as M

import Control.Monad.IO.Class (liftIO)

import Control.Exception    ( bracket )

import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )

import Data.Acid            ( AcidState, Query, Update, makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.SafeCopy        ( base, deriveSafeCopy )

import Data.Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.Aeson.Generic as AG
import Control.Applicative
import Data.Attoparsec.Number as AN
import qualified Data.Vector as V

import qualified Data.List as L
import qualified Data.Maybe as MB

import Numeric (readFloat)

import Data.Time (Day, fromGregorian , toGregorian, UTCTime(..), getCurrentTime,
                  addGregorianMonthsClip, addGregorianYearsClip)

-----------------TYPES-------------------------------
data WorkPatronage = WorkPatronage {
  work::Integer, 
  skillWeightedWork::Integer,
  seniority::Integer,
  quality::Integer,
  revenueGenerated::Integer,
  performedOver::FiscalPeriod
} deriving (Show, Eq, Ord, Data, Typeable)

data PatronageWeights = PatronageWeights {
  workw::Rational,
  skillWeightedWorkw::Rational,
  seniorityw::Rational,
  qualityw::Rational,
  revenueGeneratedw::Rational
} deriving (Show, Eq, Ord, Data, Typeable)

data MemberEquityAction = MemberEquityAction {
  actionType::EquityActionType,
  amount::Money,
  performedOn::Day
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data EquityActionType = 
  BuyIn |
  AllocatePatronageRebate | 
  DistributeImmediate | 
  DistributeInstallment |
  EarnInterest |
  DistributeOnDeparture |
  DistributeOnDissolution |
  DistributeMilestone |
  AllocateDelayedNonQualified
   deriving (Show, Read, Eq, Ord, Data, Typeable)  
     
data MemberEquityAccount = MemberEquityAccount {  
  ida::Integer,
  accountType::EquityAccountType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data EquityAccountType = Committed | RollingPatronage
   deriving (Show, Read, Eq, Ord, Data, Typeable)

data Member = Member {
  firstName::String
  --lastName::String
  --id::Integer
  --acceptedOn::Day
  --leftOn::Day
} deriving (Show, Eq, Ord, Data, Typeable)

data FinancialResults = FinancialResults { 
  over::FiscalPeriod,
  surplus::Money
  --net-inc
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data Allocation = Allocation { 
  recordedOn::Day,
  resultOf::FiscalPeriod
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data Cooperative = Cooperative {
  id::Integer,
  name::String,
  username::Email,
  bookkeeperFirstName::String,
  usageStart::Day,
  usageEnd::Day,
  fiscalCalendarType::FiscalCalendarType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

type SeniorityMapping = [(Years,Years),SeniorityLevel]
type SeniorityLevel = Integer

data FiscalPeriod = FiscalPeriod {
  start::GregorianMonth,
  periodType::PeriodType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data GregorianMonth = GregorianMonth Year Month
  deriving (Show, Read, Eq, Ord, Data, Typeable)

type Year = Integer
type Month = Int

data FiscalCalendarType = FiscalCalendarType{
  startf::Month,
  periodTypef::PeriodType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data PeriodType = Year | Quarter
  deriving (Show, Read, Eq, Ord, Data, Typeable)

type Email = String
type Money = Integer

data GregorianDuration = GregorianDuration Years Months 
  deriving (Show, Read, Eq, Ord, Data, Typeable)
type Years = Integer
type Months = Integer

type DisbursalSchedule = [(GregorianDuration, Rational)]
-- [(GregorianDuration 0 6, 1%4), (GregorianDuration 1 0, 3%4)]

type AllocationMethod = String

-----------------------UTIL-----------------------
lookString :: String -> ServerPart String
lookString = fmap unpack . lookText

lookRead :: Read a => String -> ServerPart a
lookRead = fmap read . lookString

readRational :: String -> Rational  --truncate to 100ths
readRational = toRational . fst . head . readFloat

------------------------CALCS---------------------
patronageTotal = 
  mconcat . 
    map   
      (\WorkPatronage{work=w, skillWeightedWork=sk, 
      		     seniority=sn, quality=q, revenueGenerated=r, performedOver=_} ->
         (Sum w, Sum sk, Sum sn, Sum q, Sum r))

divOrZero num 0 = 0
divOrZero num denom = num % denom

patronageProportions ps = 
  let (Sum tw, Sum tsk, Sum tsn, Sum tq, Sum tr) = patronageTotal $ M.elems ps
  in M.map 
     (\WorkPatronage{work=w,skillWeightedWork=sk,seniority=sn,
		     quality=q, revenueGenerated=r, performedOver=_} -> 
          (divOrZero w tw, divOrZero sk tsk, 
	   divOrZero sn tsn, divOrZero q tq, 
	   divOrZero r tr))
     ps

patronageAllocateRatios
  PatronageWeights{workw=ww, skillWeightedWorkw=skw, seniorityw=snw, 
  	           qualityw=qw, revenueGeneratedw=rw} = 
     M.map 
       (fromRational
       . (\(w, sk, sn, q, r) -> sum [w * ww,sk * skw,sn * snw,q * qw,r * rw]))
     . patronageProportions


allocateEquityFor FinancialResults{over=ov,surplus=sr} ps pw performedOn = 
  let memberRatios = patronageAllocateRatios pw ps
  in  
    M.map 
      (\alloc -> 
         MemberEquityAction{actionType=AllocatePatronageRebate,amount=alloc,
    	            performedOn=performedOn}) $    	       
    M.map (\proportion -> round $ proportion * toRational sr) $
    memberRatios

allMemberEquity = 
  M.map (sum . map amount)

memberEquityBalance :: [MemberEquityAction] -> Day -> Integer
memberEquityBalance actions asOf = 0

scheduleDisbursalsFor :: MemberEquityAction -> DisbursalSchedule -> [MemberEquityAction]
scheduleDisbursalsFor 
  MemberEquityAction{actionType=AllocatePatronageRebate,amount=amount, 
                     performedOn=allocatedOn} 
  schedule = 
    let addDuration (GregorianDuration years months) day = 
          addGregorianMonthsClip months $ addGregorianYearsClip years day
        makeAction dsb on = 
          MemberEquityAction{actionType=DistributeInstallment,
                             amount=dsb,
                             performedOn=on}
    in map
         (\(duration, proportion) -> 
            let disburse = round $ (toRational amount) * proportion
                on = addDuration duration allocatedOn
            in makeAction disburse on)
         schedule


------------------PERSIST---------------------------
data Globals = Globals { 
  cooperative :: Cooperative,
  settings :: Maybe (AllocationMethod, PatronageWeights, DisbursalSchedule),
  members :: [Member],
  patronage :: M.Map Member [WorkPatronage],
  accounts :: M.Map Member (M.Map MemberEquityAccount [MemberEquityAction]),
  financialResults :: [FinancialResults],
  allocations :: M.Map Allocation [MemberEquityAction]
} deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Globals)
$(deriveSafeCopy 0 'base ''Member)
$(deriveSafeCopy 0 'base ''PatronageWeights)
$(deriveSafeCopy 0 'base ''WorkPatronage)
$(deriveSafeCopy 0 'base ''FinancialResults)
$(deriveSafeCopy 0 'base ''FiscalPeriod)
$(deriveSafeCopy 0 'base ''GregorianMonth)
$(deriveSafeCopy 0 'base ''PeriodType)
$(deriveSafeCopy 0 'base ''GregorianDuration)
$(deriveSafeCopy 0 'base ''MemberEquityAction)
$(deriveSafeCopy 0 'base ''MemberEquityAccount)
$(deriveSafeCopy 0 'base ''Allocation)
$(deriveSafeCopy 0 'base ''Cooperative)
$(deriveSafeCopy 0 'base ''EquityActionType)
$(deriveSafeCopy 0 'base ''EquityAccountType)
$(deriveSafeCopy 0 'base ''FiscalCalendarType)
  
putIt :: Globals -> Update Globals Globals
putIt g = 
  do --g@Globals{members=ms,calcMethod=m,patronage=p} <- get
     --let newMem = Member $ show n
     --put $ g{members = ms ++ [newMem]}
     put g
     return g

getIt :: Query Globals Globals
getIt = 
  ask

$(makeAcidic ''Globals ['putIt, 'getIt])


--------------SERIALIZE------------------------
instance ToJSON Member where
  toJSON Member{firstName=fn} = 
  	 object ["firstName" .= fn]

instance ToJSON WorkPatronage where
  toJSON WorkPatronage{work=work,skillWeightedWork=skillWeightedWork,
		       seniority=seniority,quality=quality,
		       revenueGenerated=revenueGenerated, performedOver=prf} = 
  	 object ["work" .= work, 
	 	 "skillWeightedWork" .= skillWeightedWork, 
		 "seniority" .= seniority, "quality" .= quality,
		 "revenueGenerated" .= revenueGenerated, 
		 "performedOver" .= toJSON prf]

instance ToJSON MemberEquityAction where
  toJSON MemberEquityAction{actionType=act,amount=amt,performedOn=prf} = 
  	 object ["actionType" .= AG.toJSON act, 
	 	 "amount" .= amt, 
	 	 "performedOn" .= toGregorian prf]

instance ToJSON GregorianMonth where
  toJSON (GregorianMonth year month) = 
    	 object ["year" .= year, 
	 	 "month" .= month]

instance ToJSON FiscalPeriod where
  toJSON FiscalPeriod{start=st,periodType=pt} = 
  	 object ["start" .= toJSON st,
	  	 "periodType" .= AG.toJSON pt]

instance ToJSON FinancialResults where
  toJSON FinancialResults{over=ov,surplus=sr} = 
         object ["over" .= toJSON ov,
                 "surplus" .= sr]

instance ToJSON Allocation where
  toJSON Allocation{recordedOn=rc,resultOf=ro} = 
         object ["recordedOn" .= toGregorian rc,
                 "resultOf" .= toJSON ro]

--FromParams for PatronageWeights, WorkPatronage, FinancialResults, MemberEqAct

instance FromJSON MemberEquityAction where
  parseJSON (Object v) = 
    MemberEquityAction <$> 
      v .: "actionType" <*> 
      v .: "amount" <*> 
      v .: "performedOn"
  
instance FromJSON EquityActionType where
  parseJSON (String t) = pure $ read $ DT.unpack t

instance FromJSON GregorianMonth where
  parseJSON (Object v) = 
     GregorianMonth <$> v .: "year" <*> v.: "month"

instance FromJSON PeriodType where
  parseJSON (String t) = pure $ read $ DT.unpack t
     
instance FromJSON FiscalPeriod where
  parseJSON (Object v) = 
     FiscalPeriod <$> v .: "start" <*> v .: "periodType"

instance FromJSON Day where
  parseJSON (Array a) = 
     fromGregorian 
       <$> withNumber "inccorect num" (\(AN.I i) -> pure i) (a V.! 0)
       <*> withNumber "inccorect num" (\(AN.I i) -> pure $ fromIntegral i) (a V.! 1)
       <*> withNumber "inccorect num" (\(AN.I i) -> pure $ fromIntegral i) (a V.! 2)

---------------SERVICE------------------------
coopSummary :: PersistConnection -> ServerPartR
coopSummary ref = do 
  v <- query' ref GetIt
  ok $ toResponse $ show v

-- getFiscalPeriods

-- putCooperative :: PersistentConnection -> ServerPartR

-- getCooperative :: PersistentConnection -> ServerPartR

putMember :: PersistConnection -> ServerPartR
putMember ref = 
  do method POST
     firstName <- lookString "firstName"
     let member = Member firstName
     g <- query' ref GetIt
     let mems = members g
     g2 <- update' ref (PutIt g{members = mems ++ [member]})
     ok $ toResponse ()
     
getMembers :: PersistConnection -> ServerPartR
getMembers ref = 
  do method GET
     g <- query' ref GetIt
     let ms = members g
     ok $ toResponse $ do 
     	encode ms

getMemberPatronage :: PersistConnection -> ServerPartR
getMemberPatronage ref = 
  path $ \(idIn::String) -> dir "patronage" $ path $ \(fiscalPeriod::Integer) ->
  do method GET 
     g <- query' ref GetIt
     let ps = patronage g
     let Just m = L.find ((\i -> i == idIn). firstName) $ members g
     let Just p = M.lookup m ps
     ok $ toResponse $ do
     	encode $ head p  

-- putDefaultDisbursalSchedule
        
-- getDefaultDisbursalSchedule

-- getCalcMethod 

putCalcMethod :: PersistConnection -> ServerPartR
putCalcMethod ref = 
  path $ \(methodName::String) -> 
      do method POST 
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

putMemberPatronage :: PersistConnection -> ServerPartR
putMemberPatronage ref = 
  path $ \(idIn::String) -> dir "patronage" $ path $ \(fiscalPeriodStr::String) ->
     do method POST
	liftIO $ putStrLn $ show fiscalPeriodStr
        work <- lookRead "work"
     	skillWeightedWork <- lookRead "skillWeightedWork"
     	seniority <- lookRead "seniority"
     	quality <- lookRead "quality"
     	revenueGenerated <- lookRead "revenueGenerated"
	performedOverStr <- lookBS "performedOver" --use path instead
	let Just performedOver = decode performedOverStr
     	let p = WorkPatronage{work=work, 
	      	       skillWeightedWork=skillWeightedWork,
	 	       seniority=seniority, quality=quality,
		       revenueGenerated=revenueGenerated,performedOver=performedOver}
        g <- query' ref GetIt
     	let Just m = L.find ((\i -> i == idIn) . firstName) $ members g
     	g2 <- update' ref (PutIt g{patronage = M.insert m [p] $ patronage g})
     	ok $ toResponse ()

getAllFinancialResultsDetail :: PersistConnection -> ServerPartR
getAllFinancialResultsDetail ref = 
  do method GET
     g <- query' ref GetIt
     let res = financialResults g
     let allocs = M.keys $ allocations g
     let resDetail = 
           map 
             (\r@(FinancialResults o s) -> 
               MB.maybe (r, Nothing)
               (\a -> (r, Just a))
               (L.find (\(Allocation rc ro) -> ro == o) allocs))
             res
     ok $ toResponse $ do 
       encode resDetail
       
putFinancialResults :: PersistConnection -> ServerPartR
putFinancialResults ref = 
  do method POST
     surplus <- lookRead "surplus"
     overStr <- lookBS "over"
     liftIO $ putStrLn $ show overStr     
     let Just over = decode overStr
     let res = FinancialResults over surplus
     g <- query' ref GetIt
     let allRes = financialResults g
     _ <- update' ref (PutIt g{financialResults=allRes ++ [res]})
     ok $ toResponse ()
     

postAllocateToMembers :: PersistConnection -> ServerPartR
postAllocateToMembers ref = 
  do method POST
     surplus <- lookRead "surplus"
     overStr <- lookBS "over"
     let Just over = decode overStr
     let res = FinancialResults over surplus
     g <- query' ref GetIt
     let Just (name, parameters, _) = settings g
     UTCTime{utctDay=day,utctDayTime=_} <- liftIO getCurrentTime
     let me = allocateEquityFor res (M.map head (patronage g)) parameters day
     ok $ toResponse $ do 
     	encode $ M.toList me

putEquityAction :: PersistConnection -> ServerPartR
putEquityAction ref = 
  do method POST
     actionType <- lookRead "actionType"
     amount <- lookRead "amount"
     performedOnStr <- lookBS "performedOn"
     let Just performedOn = decode performedOnStr
     let act = MemberEquityAction{actionType=actionType,amount=amount,
     	          performedOn=performedOn}
     ok $ toResponse ()

postScheduleAllocateDisbursal :: PersistConnection -> ServerPartR
postScheduleAllocateDisbursal ref = 
  do method POST
     allocateActionStr <- lookBS "allocateAction"
     let Just allocateAction = decode allocateActionStr
     g <- query' ref GetIt
     let Just (_, _, disbursalSchedule) = settings g
     ok $ toResponse $ show $ scheduleDisbursalsFor allocateAction $ disbursalSchedule
                          
-- getEquityHistory :: PersistConnection -> ServerPartR
-- getEquitySummary 
--

--------------APP CONTROLLER------------------------
templateResponse name hState = 
  let rendered = runIdentity $ renderTemplate hState name
  in maybe (notFound $ toResponse $ "Template not found: " ++ B.unpack name)
  	   (\(bldr,_) -> ok $ toResponse $ toByteString bldr)
	   rendered
  	

---------------ENTRY---------------------------------
capaApp :: PersistConnection -> TemplateStore -> ServerPartR
capaApp ref hState = 
  msum 
  [
    dirs "control/coop/summary" $ templateResponse "coopSummary" hState
  , dirs "control/member/accounts" $ templateResponse "memberAccounts" hState
  , dirs "control/financial/results" $ templateResponse "financialResults" hState
  , dirs "control/members/patronage/period" $ templateResponse "periodPatronage" hState
  , dirs "control/equity/members/allocationsDisbursals" $ 
           templateResponse "allocationsDisbursals" hState
  , dirs "control/members/add" $ templateResponse "newMember" hState
  , dirs "control/member/account/action/add" $ templateResponse "addAction" hState
  , dirs "control/financial/result/record" $ templateResponse "recordResult" hState
  , dirs "control/member/patronage/record" $ templateResponse "recordPatronage" hState
  , dirs "control/coop/register" $ templateResponse "registerCoop" hState
  , dirs "control/coop/settings" $ templateResponse "coopSettings" hState
  
  , dir "members" $ getMembers ref
  , dir "member" $ putMember ref
  , dirs "financial/results" $ getAllFinancialResultsDetail ref
  , dirs "financial/results" $ putFinancialResults ref
  , dirs "surplus/allocate/method" $ putCalcMethod ref
  , dir "member" $ putMemberPatronage ref
  , dir "member" $ getMemberPatronage ref
  , dirs "equity/members/allocate" $ postAllocateToMembers ref
  , dirs "member/equity/disburse" $ postScheduleAllocateDisbursal ref
  , dirs "member/equity/history" $ putEquityAction ref
  , coopSummary ref]

type PersistConnection = AcidState Globals
type TemplateStore = HeistState Identity
type ServerPartR = ServerPart Response

main = do
   let g0 = Globals 
              (Cooperative 
                  1 "Coop1" "k@m.com" "John" 
                  (fromGregorian 2010 1 1) 
                  (fromGregorian 2010 2 2) (FiscalCalendarType 1 Year))
              Nothing -- [(GregorianDuration 0 3, 1%4),(GregorianDuration 1 6, 3%4)] 
              []
              M.empty
              M.empty
              [FinancialResults (FiscalPeriod (GregorianMonth 2012 1) Year) 200] 
              (M.fromList [(Allocation 
                              (fromGregorian 2011 1 2) 
                              (FiscalPeriod (GregorianMonth 2012 1) Year), [])])
   x <- openLocalState g0
   ehs <- runEitherT $ do 
     templateRepo <- loadTemplates "control"
     let hCfg = (HeistConfig 
	           [] 
	           (defaultInterpretedSplices ++ defaultLoadTimeSplices) 
	           [] 
	           [] 
	           templateRepo)::HeistConfig Identity
     initHeist hCfg
   either (error . concat) (serve Nothing . capaApp x) ehs 

{---------------TODO------------------
-refine types and calcs more ~ a little
-expand serialize, services ~ halfway
-implement form behavior, add datepicker

-use across different date ranges
-filter by coop

-authenticate + user session
-persist postgres
-polish UI
-multiple source files/modules
-automate test(+travis), run, release, refresh. setup hosting. document.
-filter deactivated members, accounts
-use sets instead of lists

-configuration. automate backups, restore. 
-admin UI, export method
-(sys)logging, automate monitor.
-req + spec docs/website. bugs + enhance tracker. training tutorials.
-javascript reorganize, library survey
-remote support
-creation/install and vm of prod env
-better error with dead end web paths
-meta: pricing/donations/developer and financial health monitor
-interest daily job
-patronage - salary
-}
