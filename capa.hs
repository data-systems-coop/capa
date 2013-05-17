{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, 
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
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L

data WorkPatronage = WorkPatronage { --validate/make
  work::Integer, 
  skillWeightedWork::Integer,
  seniority::Integer,
  quality::Integer,
  revenueGenerated::Integer
  --performedOver::FiscalPeriod
} deriving (Show, Eq, Ord, Data, Typeable)

instance ToJSON WorkPatronage where
  toJSON (WorkPatronage{work=w,skillWeightedWork=sw,seniority=sn,quality=q,
			revenueGenerated=r}) = 
	object ["work" .= w, "skillWeightedWork" .= sw]

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
   deriving (Show)  
     
data Member = Member {
  firstName::String
} deriving (Show, Eq, Ord, Data, Typeable)

data FinancialResults = FinancialResults { 
  over::Integer, --FiscalPeriod  
  surplus::Integer
} deriving (Show)

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

--date representation

--put /member firstName=<name>
--get /members
--put /patronage/calcMethod/<method name> workw=<..> & ...
--put /member/equityAction  actionType=<...> & ...

--postgresql db

{- state??  members. method + weights. mem->patronage -}
data Globals = Globals { 
  members :: [Member],
  calcMethod :: (String, PatronageWeights),
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

--homepage :: ServerPart Response 
homepage ref = do 
  --c <- update' ref (IncIt 20)
  v <- query' ref GetIt
  ok $ toResponse $ do 
     show "hi"

--patronageMethod :: ServerPart Response
patronageMethod = 
 do method POST
    hours1 <- lookText "hours1"
    wages1 <- lookText "wages1"
    hours2 <- lookText "hours2"
    wages2 <- lookText "wages2"
    let p0 = WorkPatronage{work=10,skillWeightedWork=20,seniority=2,quality=4,
				revenueGenerated=4}
    --let p1 = WorkPatronage{work=read $ unpack hours1, revenue=read $ unpack wages1}
    --let p2 = WorkPatronage{work=read $ unpack hours2, revenue=read $ unpack wages2}
    ok $ toResponse $
       show $ B.unpack $ encode p0 -- hours1 patronageTotal [p1, p2] 

--put /member/<id>/patronage/<fiscalPeriod> work=<...> & 
memberPatronageMethod ref = 
  do method PUT 
     idIn <- lookText "id"
     work <- lookText "work"
     skillWeightedWork <- lookText "skillWeightedWork"
     seniority <- lookText "seniority"
     quality <- lookText "quality"
     revenueGenerated <- lookText "revenueGenerated"
     let readun = read . unpack
     let p = WorkPatronage{work=readun work, skillWeightedWork=readun skillWeightedWork,
	 	       seniority=readun seniority, quality=readun quality,
		       revenueGenerated=readun revenueGenerated}
     g <- query' ref GetIt
     let Just m = L.find ((\i -> i == unpack idIn). firstName) $ members g
     g2 <- update' ref (PutIt g{patronage = M.insert m p $ patronage g})
     ok $ toResponse $ do
     	show g2
     	
--post /equityAllocation ? surplus=<...>
equityAllocation ref = 
  do method POST
     surplus <- lookText "surplus"
     let res = FinancialResults 1 $ read $ unpack surplus
     g <- query' ref GetIt
     let e = allocateEquityFor res (patronage g) (snd $ calcMethod g)
     ok $ toResponse $ do 
     	show e

--capaApp :: ServerPart Response
capaApp ref = 
  msum 
  [ dir "patronage" $ patronageMethod 
  , dir "member" $ dir "patronage" $ memberPatronageMethod ref
  , dir "equityAllocation" $ equityAllocation ref
  , homepage ref
  ]

main = do
   let wghts = PatronageWeights{workw=3%5,skillWeightedWorkw=0,seniorityw=1%5,
		qualityw=0,revenueGeneratedw=1%5}
   let g0 = Globals [Member "a", Member "b"] ("regular", wghts) M.empty
   x <- openLocalState g0
   serve Nothing $ capaApp x

