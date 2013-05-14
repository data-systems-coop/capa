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

data WorkPatronage = WorkPatronage { --validate/make
  work::Integer, 
  skillWeightedWork::Integer,
  seniority::Integer,
  quality::Integer,
  revenueGenerated::Integer
  --performedOver::FiscalPeriod
} deriving (Show)

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
} deriving (Show)

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
} deriving (Show, Eq, Ord)

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

--parse from json to values, output to json

--date representation

--put /member firstName=<name>
--get /members
--put /patronage/calcMethod/<method name> workw=<..> & ...
--put /member/<id>/patronage/<fiscalPeriod> work=<...> & 
--post /equityAllocation ? surplus=<...>
--put /member/equityAction  actionType=<...> & ...

--postgresql db

{- state??  members. method + weights. mem->patronage -}
data Globals = Globals Integer
  deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Globals)

incIt :: Integer -> Update Globals Integer
incIt n = 
  do Globals i <- get
     let newn = i + n
     put $ Globals newn
     return newn

$(makeAcidic ''Globals ['incIt])

--homepage :: ServerPart Response 
homepage ref = do 
  c <- update' ref (IncIt 20)
  ok $ toResponse $ do 
     show c

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

--capaApp :: ServerPart Response
capaApp ref = 
  msum 
  [ dir "patronage" $ patronageMethod 
  , homepage ref ]

main = do
   x <- openLocalState $ Globals 10
   serve Nothing $ capaApp x

{-
main =
  let pa1 = 
       WorkPatronage{work=10, skillWeightedWork=0, seniority=2, quality=1, 
       	         revenueGenerated=100}
      pa2 = 
       WorkPatronage{work=4, skillWeightedWork=0, seniority=3, quality=3,
       	         revenueGenerated=100}
      pw =
       PatronageWeights{workw=3%5,skillWeightedWorkw=0,seniorityw=1%5,qualityw=0,
		 revenueGeneratedw=1%5}
      e1 = MemberEquityAction{actionType=BuyIn,amount=1000,performedOn=20,resultOf=1}
      e2 = 
        MemberEquityAction{actionType=AllocatePatronageRebate,amount=1000,
		performedOn=20,resultOf=1}
      m1 = Member{firstName="m1"}
      m2 = Member{firstName="m2"}
      pams = M.fromList [(m1,pa1),(m2,pa2)]
      rs = FinancialResults{over=10,surplus=3000}
  in do 
    putStrLn $ show pams
    putStrLn $ show $ patronageAllocateRatios pw pams
    putStrLn $ show $ allocateEquityFor rs pams pw
    putStrLn $ show $ allMemberEquity $ M.fromList [(m1,[e1,e2]), (m2,[e1])]
-}

