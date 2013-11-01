module Domain 
where 

import Types  
import qualified Data.Map as M 
import Data.Time (Day, addGregorianMonthsClip, addGregorianYearsClip)
import Data.Ratio ((%))
import Data.Monoid (mconcat, Sum(..)) 
import Data.List
import Control.Monad.Reader


patronageComponents :: 
  (Member, WorkPatronage) -> 
    Reader 
      (SeniorityMappings, Day)
      (Sum Integer, Sum Integer, Sum Integer, Sum Integer, Sum Integer)
patronageComponents 
  (mbr, WorkPatronage{work=w, skillWeightedWork=sk, quality=q, revenueGenerated=r}) = do
    (snrtyMap, asOf) <- ask
    let sn = mbrSeniorityLevel mbr asOf snrtyMap
    return (Sum w, Sum sk, Sum sn, Sum q, Sum r)

patronageComponentTotals ::
  [(Member, WorkPatronage)] -> 
    Reader (SeniorityMappings, Day) (Integer, Integer, Integer, Integer, Integer)
patronageComponentTotals wps = do
  (Sum w, Sum sk, Sum sn, Sum q, Sum r) <- fmap mconcat $ mapM patronageComponents wps
  return (w, sk, sn, q, r)

memberPatronageComponentProportions :: 
  M.Map Member WorkPatronage ->  
    Reader 
      (SeniorityMappings, Day)
      (M.Map Member (Rational, Rational, Rational, Rational, Rational))
memberPatronageComponentProportions ps = do
  (tw, tsk, tsn, tq, tr) <- patronageComponentTotals $ M.toList ps  
  let componentProportions 
        (mbr,WorkPatronage{work=w,skillWeightedWork=sk,quality=q, revenueGenerated=r}) =
          do
           (snrtyMap, asOf) <- ask
           let sn = mbrSeniorityLevel mbr asOf snrtyMap
           return 
             (mbr, (w `div'` tw, sk `div'` tsk,sn `div'` tsn, q `div'` tq, r `div'` tr))
  fmap M.fromList $ mapM componentProportions $ M.toList ps
  where 
    _ `div'` 0 = 0
    n `div'` d = n % d

memberPatronageAllocateRatios :: 
  PatronageWeights -> M.Map Member WorkPatronage -> 
    Reader (SeniorityMappings, Day) (M.Map Member Rational)
memberPatronageAllocateRatios 
  PatronageWeights{workw=ww, skillWeightedWorkw=skw, seniorityw=snw, 
  	           qualityw=qw, revenueGeneratedw=rw} ps = do 
    pc <- memberPatronageComponentProportions ps 
    return $ truncateAll $ M.map weightedSum pc
  where 
    truncateAll mp = 
      let fixed = truncateOverflow $ M.elems mp
      in M.fromList $ zip (M.keys mp) fixed
    weightedSum (w, sk, sn, q, r) = sum [w * ww,sk * skw,sn * snw,q * qw,r * rw]

allocateEquityFor ::
  FinancialResults -> Day -> (PatronageWeights, M.Map Member WorkPatronage) -> 
    Reader SeniorityMappings (M.Map Member (MemberEquityAction, Rational))
allocateEquityFor 
  FinancialResults{over=FiscalPeriod{start=start}, surplus=surplus} 
  performedOn 
  (wghts, memPatr) = do 
    memRatios <- 
     withReader (\mp -> (mp, toDay start)) $ memberPatronageAllocateRatios wghts memPatr
    return $ M.mapWithKey (\m amt -> (makeAlloc amt, memRatios M.! m)) $
      M.map allocateSurplus $ memRatios
    where makeAlloc amt = 
            MemberEquityAction{actionType=AllocatePatronageRebate, amount=amt,
                               performedOn=performedOn}
          allocateSurplus = round . ((toRational surplus) *)

scheduleDisbursalsFor :: MemberEquityAction -> DisbursalSchedule -> [MemberEquityAction]
scheduleDisbursalsFor 
  MemberEquityAction{actionType=AllocatePatronageRebate,amount=amount, 
                     performedOn=allocatedOn} = 
  map (\(after, proportion) -> makeDisburse (allocatedOnPlus after) $ disbursed proportion)
  where 
    allocatedOnPlus = gregorianDayPlus allocatedOn 
    disbursed = round . ((toRational amount) *)
    makeDisburse on amt = 
      MemberEquityAction{actionType=DistributeInstallment,amount=amt,performedOn=on}

scheduleDisbursals :: Day -> DisbursalSchedule -> [Disbursal]
scheduleDisbursals allocatedOn = 
  map 
    (\(after, proportion) -> 
      Disbursal{dsbPerformedOn = gregorianDayPlus allocatedOn after, 
                dsbProportion = proportion})
  
gregorianDayPlus :: Day -> GregorianDuration -> Day
gregorianDayPlus day (GregorianDuration years months) = 
  addGregorianMonthsClip months $ addGregorianYearsClip years day

runningBalance :: [MemberEquityAction] -> [(MemberEquityAction, Money)]
runningBalance acns = zip acns $ scanl1 (+) $ fmap acnEffectiveAmount acns

truncateOverflow :: [Rational] -> [Rational]
truncateOverflow proportions = 
  let (truncated, leftover) = 
        foldl 
          (\(truncated, leftover) proportion ->
            let (n,f) = properFraction (proportion * (100 % 1))
            in (truncated ++ [n % 100], leftover + (f / 100)))
          ([], 0)
          proportions
  in (init truncated) ++ [(last truncated + leftover)]