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
  surplus::Money,   --net-inc
  allocatedOn::Maybe Day
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

readRational :: String -> Rational  -- round to 100ths
readRational = toRational . fst . head . readFloat

newtype JSONData a = JSONData{ getJSONData :: a }

instance ToJSON a => ToMessage (JSONData a) where
  toMessage (JSONData d) = encode d
  toContentType _ = B.pack ("application/json")
  

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
  allocations :: M.Map FinancialResults [MemberEquityAction]
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
  toJSON FinancialResults{over=ov,surplus=sr,allocatedOn=ao} = 
         object ["over" .= toJSON ov,
                 "surplus" .= sr, 
                 "allocatedOn" .= toJSON ao] --just then toGregorian, nothing then null

instance ToJSON Day where
  toJSON d = 
    let (yr,mo,dy) = toGregorian d
    in Array $ V.fromList [toJSON yr,toJSON mo,toJSON dy]

--FromQParams for PatronageWeights, WorkPatronage, FinancialResults, MemberEqAct

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

-- getFiscalPeriods :: PersistentConnectin -> ServerPartR

-- putCooperative :: PersistentConnection -> ServerPartR
-- getCooperative :: PersistentConnection -> ServerPartR


putMember :: PersistConnection -> ServerPartR
putMember ref = -- get all parameters for member
  do firstName <- lookString "firstName"
     let member = Member firstName
     g <- query' ref GetIt
     let mems = members g
     g2 <- update' ref (PutIt g{members = mems ++ [member]})
     ok $ toResponse ()
     
-- detail
getMembers :: PersistConnection -> ServerPartR
getMembers ref = -- get sum of equity balances with each member
  do g <- query' ref GetIt
     let ms = members g
     ok $ toResponse $ JSONData ms

-- putMemberRquityAccount


-- getMemberEquityAccounts
        


getMemberPatronage :: PersistConnection -> ServerPartR
getMemberPatronage ref =  -- replace with get all for period
  path $ \(idIn::String) -> dir "patronage" $ path $ \(fiscalPeriod::Integer) ->
  do g <- query' ref GetIt
     let ps = patronage g
     let Just m = L.find ((\i -> i == idIn). firstName) $ members g
     let Just p = M.lookup m ps
     ok $ toResponse $ JSONData $ head p  


-- putDefaultDisbursalSchedule
-- getDefaultDisbursalSchedule

-- getCalcMethod
putCalcMethod :: PersistConnection -> ServerPartR
putCalcMethod ref = 
  path $ \(methodName::String) -> 
      do let lookRational = fmap readRational . lookString
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
     do liftIO $ putStrLn $ show fiscalPeriodStr
        work <- lookRead "work"
     	skillWeightedWork <- lookRead "skillWeightedWork"
     	seniority <- lookRead "seniority"
     	quality <- lookRead "quality"
     	revenueGenerated <- lookRead "revenueGenerated"
	performedOverStr <- lookBS "performedOver" -- use path instead
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
  do g <- query' ref GetIt
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
     surplus <- lookRead "surplus"
     overStr <- lookBS "over"
     let Just over = decode overStr
     let res = FinancialResults over surplus $ Just day
     g <- query' ref GetIt
     let Just (name, parameters, _) = settings g
     let me = allocateEquityFor res (M.map head (patronage g)) parameters day
     ok $ toResponse $ JSONData $ M.toList me

-- postAllocationDisbursal
    -- save allocation entry, all allocs, all distribs    
        

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
                          


--------------APP CONTROLLER------------------------
templateResponse name hState = 
  nullDir >> method GET >>
  let rendered = runIdentity $ renderTemplate hState name
  in maybe (notFound $ toResponse $ "Template not found: " ++ B.unpack name)
  	   (\(bldr,_) -> ok $ toResponse $ toByteString bldr)
	   rendered
  	

---------------ENTRY---------------------------------
capaApp :: PersistConnection -> TemplateStore -> ServerPartR
capaApp ref hState = msum [
    dir "control" $ msum [
         dir "coop" $ msum [
            dir "summary" $ templateResponse "coopSummary" hState
          , dir "register" $ templateResponse "registerCoop" hState
          , dir "settings" $ templateResponse "coopSettings" hState ]
       , dir "member" $ msum [
            dir "accounts" $ templateResponse "memberAccounts" hState
          , dir "account" $ dir "action" $ dir "add" $ 
              templateResponse "addAction" hState
          , dir "patronage" $ dir "record" $ 
              templateResponse "recordPatronage" hState ]
       , dir "members" $ msum [
             dir "patronage" $ dir "period" $ templateResponse "periodPatronage" hState
           , dir "add" $ templateResponse "newMember" hState ]
       , dir "financial" $ dir "results" $ msum [
             templateResponse "financialResults" hState
           , dir "record" $ templateResponse "recordResult" hState ]
       , dirs "equity" $ msum [ 
            dir "members"  $ dir "allocationsDisbursals" $ 
              templateResponse "allocationsDisbursals" hState ] ]
  , dir "financial" $ dir "results" $ msum [ 
       method GET >> getAllFinancialResultsDetail ref
     , method POST >> putFinancialResults ref ]
  , dirs "surplus/allocate/method" $ method POST >> putCalcMethod ref
  , dir "members" $ method GET >> getMembers ref  
  , dir "member" $ msum [ 
         method POST >> putMember ref
       , method POST >> putMemberPatronage ref
       , method GET >> getMemberPatronage ref 
       , dir "equity" $ msum [
           dir "disburse" $ method POST >> postScheduleAllocateDisbursal ref
         , dir "history" $ method POST >> putEquityAction ref ] ]
  , dir "equity" $ msum [
       dir "members" $ msum [
         dir "allocate" $ method POST >> postAllocateToMembers ref ] ]
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
              [FinancialResults 
                  (FiscalPeriod (GregorianMonth 2012 1) Year) 
                  200
                  $ Just (fromGregorian 2011 1 2)] 
              (M.fromList [(FinancialResults  
                              (FiscalPeriod (GregorianMonth 2012 1) Year)
                              200
                              $ Just (fromGregorian 2011 1 2), 
                            [])])
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
-implement expanded services 
-implement form behavior, finish edits and pickers

-refine types, calcs
-use across different date ranges
-add coop parameter

-authenticate, session
-postgres, liquibase
-modules
-automate test w/travis
-automate run. kill
-automate release, refresh
-select hosting
-filter deactivated members, accounts
-sets not lists
-url paths, template naming conventions

-config
-automate backup, restore
-export procedure
-(sys)log
-create monitoring, schedule backup
-req, spec wiki
-bugs, enhance tracker
-user manual, dev manual
-javascript reorganize, widget and binding library survey
-remote support procedures
-create prod env vm with bootstrap script
-handle partial path fail
-meta: cost estimate/donation/developer avail and financial health monitor
-interest calc daily job
-}
