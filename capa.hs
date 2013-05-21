{-# Language OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, 
    	     TemplateHaskell, TypeFamilies #-}
module Main (main) where
import Happstack.Lite
import Text.Blaze.Html5 (p)
import Data.Text.Lazy (unpack)
import Data.Monoid
import Data.Ratio ((%))
--import Data.Default 
import qualified Data.Map as M
import Data.IORef
import Control.Exception    ( bracket )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Data.Acid            ( AcidState, Query, Update, makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Aeson
import qualified Data.Aeson.Generic as AG
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import Numeric (readFloat)

-----------------TYPES-------------------------------
data WorkPatronage = WorkPatronage { --validate/make
  work::Integer, 
  skillWeightedWork::Integer,
  seniority::Integer,
  quality::Integer,
  revenueGenerated::Integer
  --performedOver::FiscalPeriod
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
  amount::Integer,
  performedOn::Integer,
  resultOf::Integer   --FiscalPeriod
} deriving (Show)

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
   deriving (Show, Read, Data, Typeable)  
     
data Member = Member {
  firstName::String
} deriving (Show, Eq, Ord, Data, Typeable)

data FinancialResults = FinancialResults { 
  over::Integer, --FiscalPeriod  
  surplus::Integer
} deriving (Show)

--date representation

-----------------------UTIL-----------------------
lookString :: String -> ServerPart String
lookString = fmap unpack . lookText

lookRead :: Read a => String -> ServerPart a
lookRead = fmap read . lookString

readRational :: String -> Rational
readRational = toRational . fst . head . readFloat

------------------------CALCS---------------------
patronageTotal = 
  mconcat . 
    map   
      (\WorkPatronage{work=w, skillWeightedWork=sk, 
      		     seniority=sn, quality=q, revenueGenerated=r} ->
         (Sum w, Sum sk, Sum sn, Sum q, Sum r))

divOrZero num 0 = 0
divOrZero num denom = num % denom

patronageProportions ps = 
  let (Sum tw, Sum tsk, Sum tsn, Sum tq, Sum tr) = patronageTotal $ M.elems ps
  in M.map 
     (\WorkPatronage{work=w,skillWeightedWork=sk,seniority=sn,
		     quality=q, revenueGenerated=r} -> 
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


allocateEquityFor FinancialResults{over=ov,surplus=sr} ps pw = 
  let memberRatios = patronageAllocateRatios pw ps
  in  
   map
      (\alloc -> 
         MemberEquityAction{actionType=AllocatePatronageRebate,amount=alloc,
    	            performedOn=0,resultOf=ov})    	       
    . M.elems 
    . M.map (\proportion -> round $ proportion * toRational sr) $
    memberRatios

allMemberEquity = 
  M.map (sum . map amount)



------------------PERSIST---------------------------
data Globals = Globals { 
  members :: [Member],
  calcMethod :: Maybe (String, PatronageWeights),
  patronage :: M.Map Member WorkPatronage
} deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Globals)
$(deriveSafeCopy 0 'base ''Member)
$(deriveSafeCopy 0 'base ''PatronageWeights)
$(deriveSafeCopy 0 'base ''WorkPatronage)

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

--postgresql db


--------------SERIALIZE------------------------
instance ToJSON Member where
  toJSON Member{firstName=fn} = 
  	 object ["firstName" .= fn]

instance ToJSON MemberEquityAction where
  toJSON MemberEquityAction{actionType=act,amount=amt,performedOn=prf,resultOf=res} = 
  	 object ["actionType" .= AG.toJSON act, "amount" .= amt, "performedOn" .= prf,
	 	 "resultOf" .= res]

--FromParams for PatronageWeights, WorkPatronage, FinancialResults, MemberEqAct

---------------SERVICE------------------------
--coopSummary :: ServerPart Response 
coopSummary ref = do 
  v <- query' ref GetIt
  ok $ toResponse $ do 
     show v

putMember ref = 
  do method PUT
     firstName <- lookString "firstName"
     let member = Member firstName
     g <- query' ref GetIt
     let mems = members g
     g2 <- update' ref (PutIt g{members = mems ++ [member]})
     ok $ toResponse ()
     
getMembers ref = 
  do method GET
     g <- query' ref GetIt
     let ms = members g
     ok $ toResponse $ do 
     	show $ B.unpack $ encode ms

putCalcMethod ref = 
  path $ \(methodName::String) -> 
      do method PUT
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
     	 g2 <- update' ref $ PutIt g{calcMethod = Just (methodName, pw)}
     	 ok $ toResponse ()

putMemberPatronage ref = 
  path $ \(idIn::String) -> dir "patronage" $ path $ \(fiscalPeriod::Integer) ->
     do method PUT 
     	work <- lookRead "work"
     	skillWeightedWork <- lookRead "skillWeightedWork"
     	seniority <- lookRead "seniority"
     	quality <- lookRead "quality"
     	revenueGenerated <- lookRead "revenueGenerated"
     	let p = WorkPatronage{work=work, 
	      	       skillWeightedWork=skillWeightedWork,
	 	       seniority=seniority, quality=quality,
		       revenueGenerated=revenueGenerated}
        g <- query' ref GetIt
     	let Just m = L.find ((\i -> i == idIn). firstName) $ members g
     	g2 <- update' ref (PutIt g{patronage = M.insert m p $ patronage g})
     	ok $ toResponse ()
     	
postAllocateToMembers ref = 
  do method POST
     surplus <- lookRead "surplus"
     let res = FinancialResults 1 surplus
     g <- query' ref GetIt
     let Just (name, parameters) = calcMethod g
     let e = allocateEquityFor res (patronage g) parameters
     ok $ toResponse $ do 
     	show $ B.unpack $ encode e

putEquityAction ref = 
  do method PUT
     actionType <- lookRead "actionType"
     amount <- lookRead "amount"
     performedOn <- lookRead "performedOn"
     resultOf <- lookRead "resultOf"
     let act = MemberEquityAction{actionType=actionType,amount=amount,
     	          performedOn=performedOn, resultOf=resultOf}
     ok $ toResponse ()

---------------SERVICE CONTROLLER------------------
--capaApp :: ServerPart Response
capaApp ref = 
  msum 
  [ --Getting started page with: calc method, weights
    --Summary page with links to below
     --Review members page with add member form
     --Enter patronage form
     --Allocate page with surplus box and action to use allocation
    dir "members" $ getMembers ref
  , dir "member" $ putMember ref
  , dir "surplus" $ dir "allocate" $ dir "method" $ putCalcMethod ref
  , dir "member" $ putMemberPatronage ref
  , dir "equity" $ dir "members" $ dir "allocate" $ postAllocateToMembers ref
  , dir "member" $ dir "equity" $ dir "history" $ putEquityAction ref
  , coopSummary ref]


---------------ENTRY---------------------------------
main = do
   let g0 = Globals [] Nothing M.empty
   x <- openLocalState g0
   serve Nothing $ capaApp x


{---------------TODO------------------
-put minimal UI up <<<< (serve skeleton pages, ajax on load, inject)
-refine types and calcs more (+ Date) 
-persist postgres
-expand types
-expand serialize
-expand services
-authenticate
-polish UI
-automate test(+travis), run, release, refresh. setup hosting. document.
-configuration. automate backups, restore.
-(sys)logging, automate monitor.
-req + spec docs/website. bugs + enhance tracker. training tutorials.
-javascript reorganize, library survey
-remote support
-creation/install and vm of prod env
-perf test
-meta: pricing/donations/developer and financial health monitor
-}