module Domain 
where 

import Types  
import qualified Data.Map as M 
import Data.Time (Day, addGregorianMonthsClip, addGregorianYearsClip)
import Data.Ratio ((%))
import Data.Monoid (mconcat, Sum(..)) 



patronageComponents :: 
  WorkPatronage -> (Sum Integer, Sum Integer, Sum Integer, Sum Integer, Sum Integer)
patronageComponents 
  WorkPatronage{work=w, skillWeightedWork=sk, 
                seniority=sn, quality=q, revenueGenerated=r} = 
    (Sum w, Sum sk, Sum sn, Sum q, Sum r)

patronageComponentTotals :: --(patr,seniority level) 
  [WorkPatronage] -> (Integer, Integer, Integer, Integer, Integer)
patronageComponentTotals wps = (w, sk, sn, q, r)
  where (Sum w, Sum sk, Sum sn, Sum q, Sum r) = mconcat $ map patronageComponents wps

memberPatronageComponentProportions :: 
  M.Map Member WorkPatronage ->  
    M.Map Member (Rational, Rational, Rational, Rational, Rational)
memberPatronageComponentProportions ps = 
  M.map componentProportions ps
  where 
    componentProportions 
      WorkPatronage{work=w,skillWeightedWork=sk,seniority=sn,
		    quality=q, revenueGenerated=r} = 
        (w `div'` tw, sk `div'` tsk,sn `div'` tsn, q `div'` tq, r `div'` tr)        
    (tw, tsk, tsn, tq, tr) = patronageComponentTotals $ M.elems ps
    _ `div'` 0 = 0
    n `div'` d = n % d

memberPatronageAllocateRatios :: 
  PatronageWeights -> M.Map Member WorkPatronage -> M.Map Member Rational
memberPatronageAllocateRatios 
  PatronageWeights{workw=ww, skillWeightedWorkw=skw, seniorityw=snw, 
  	           qualityw=qw, revenueGeneratedw=rw} = 
     M.map weightedSum . memberPatronageComponentProportions
  where weightedSum (w, sk, sn, q, r) = sum [w * ww,sk * skw,sn * snw,q * qw,r * rw]

allocateEquityFor ::
  FinancialResults -> Day -> (PatronageWeights, M.Map Member WorkPatronage) -> 
    M.Map Member MemberEquityAction
allocateEquityFor FinancialResults{surplus=surplus} performedOn = 
  M.map makeAlloc . M.map allocateSurplus . uncurry memberPatronageAllocateRatios
  where makeAlloc amt = 
          MemberEquityAction{actionType=AllocatePatronageRebate, amount=amt,
                             performedOn=performedOn}
        allocateSurplus = round . ((toRational surplus) *)

scheduleDisbursalsFor :: MemberEquityAction -> DisbursalSchedule -> [MemberEquityAction]
scheduleDisbursalsFor 
  MemberEquityAction{actionType=AllocatePatronageRebate,amount=amount, 
                     performedOn=allocatedOn} = 
  map (\(after, proportion) -> makeDisburse (allocatedOn+ after) $ disbursed proportion)
  where 
    allocatedOn+ (GregorianDuration years months) = 
      addGregorianMonthsClip months $ addGregorianYearsClip years allocatedOn
    disbursed = round . ((toRational amount) *)
    makeDisburse on amt = 
      MemberEquityAction{actionType=DistributeInstallment,amount=amt,performedOn=on}

runningBalance :: [MemberEquityAction] -> [(MemberEquityAction, Money)]
runningBalance acns = zip acns $ scanl1 (+) $ fmap amount acns
