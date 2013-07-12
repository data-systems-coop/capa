module Domain 
where 

import Types  
import qualified Data.Map as M 
import Data.Time (Day, addGregorianMonthsClip, addGregorianYearsClip)
import Data.Ratio ((%))
import Data.Monoid (mconcat, Sum(..)) 

--patr + seniority level

patronageTotal :: 
  [WorkPatronage] -> (Sum Integer, Sum Integer, Sum Integer, Sum Integer, Sum Integer)
patronageTotal = 
  mconcat . 
    map   
      (\WorkPatronage{work=w, skillWeightedWork=sk, 
      		     seniority=sn, quality=q, revenueGenerated=r, performedOver=_} ->
         (Sum w, Sum sk, Sum sn, Sum q, Sum r))

divOrZero :: Integer -> Integer -> Rational
divOrZero num 0 = 0
divOrZero num denom = num % denom

patronageProportions :: 
  M.Map Member WorkPatronage -> 
    M.Map Member (Rational, Rational, Rational, Rational, Rational)
patronageProportions ps = 
  let (Sum tw, Sum tsk, Sum tsn, Sum tq, Sum tr) = patronageTotal $ M.elems ps
  in M.map 
     (\WorkPatronage{work=w,skillWeightedWork=sk,seniority=sn,
		     quality=q, revenueGenerated=r, performedOver=_} -> 
          (divOrZero w tw, divOrZero sk tsk, 
	   divOrZero sn tsn, divOrZero q tq, 
	   divOrZero r tr))
     ps

patronageAllocateRatios :: 
  PatronageWeights -> M.Map Member WorkPatronage -> M.Map Member Rational
patronageAllocateRatios 
  PatronageWeights{workw=ww, skillWeightedWorkw=skw, seniorityw=snw, 
  	           qualityw=qw, revenueGeneratedw=rw} = 
     M.map 
       (fromRational
       . (\(w, sk, sn, q, r) -> sum [w * ww,sk * skw,sn * snw,q * qw,r * rw]))
     . patronageProportions

allocateEquityFor ::
  FinancialResults -> M.Map Member WorkPatronage -> PatronageWeights -> Day -> 
    M.Map Member MemberEquityAction
allocateEquityFor FinancialResults{over=ov,surplus=sr} ps pw performedOn = 
  let memberRatios = patronageAllocateRatios pw ps
  in  
    M.map 
      (\alloc -> 
         MemberEquityAction{actionType=AllocatePatronageRebate,amount=alloc,
    	            performedOn=performedOn}) $    	       
    M.map (\proportion -> round $ proportion * toRational sr) $
    memberRatios

allMemberEquity :: M.Map Member [MemberEquityAction] -> M.Map Member Money
allMemberEquity = 
  M.map (sum . map amount)

memberEquityBalance :: [MemberEquityAction] -> Day -> Money
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

