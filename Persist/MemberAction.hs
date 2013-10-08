module Persist.MemberAction
where
  
import Persist.Base  
import Persist.Member

import qualified Data.Map as M

acnSaveFor :: 
  Connection -> Integer -> Integer -> Integer -> Maybe FiscalPeriod 
  -> MemberEquityAction -> IO ()
acnSaveFor 
  dbCn cpId mbrId acctId resultOf 
  MemberEquityAction{actionType=tp,amount=amt,performedOn=prf} = do 
    let (prdStartDay, prdType) = maybe (SqlNull, SqlNull) prdToSql resultOf
    run dbCn "\
      \insert into MemberEquityAction \
      \values(\
      \   ?,\
      \   ?,\
      \   ?,\
      \   ?,\
      \   ?,\
      \   ?,\
      \   (?,?))"
      [SqlInteger cpId, SqlInteger mbrId, SqlInteger acctId, 
       SqlString $ show tp, SqlInteger amt, SqlLocalDate prf,
       prdStartDay, prdType]
    commit dbCn
    
allocAcnSaveToRolling :: 
  Connection -> Integer -> Integer -> FiscalPeriod -> Rational
  -> IO ()  
allocAcnSaveToRolling dbCn cpId mbrId resultOf allocatedRatio = do
  ((acctId:_):_) <- 
    quickQuery' dbCn "\
        \select acctId \
        \from MemberEquityAccount \
        \where (cpId,mbrId,acctType) \
        \     = (?,?,?)" 
      [SqlInteger cpId, SqlInteger mbrId, 
       SqlString $ show RollingPatronageAcct]
  let (prdStartDay, prdType) = prdToSql resultOf
  run dbCn "\
    \insert into MemberAllocateAction \
    \values(\
    \   ?,\
    \   ?,\
    \   ?,\
    \   ?,\
    \   (?,?))"
    [SqlInteger cpId, SqlInteger mbrId, acctId, 
     toSqlDouble allocatedRatio, prdStartDay, prdType]
  commit dbCn
    
acnFrom :: [SqlValue] -> MemberEquityAction 
acnFrom (acnType:amnt:perfOn:_) = 
  MemberEquityAction (read $ fromSql acnType) (fromSql amnt) (fromSql perfOn)

acnGetFor :: 
  Connection -> Integer -> Integer -> Integer -> 
    IO [(MemberEquityAction, Maybe FiscalPeriod)]
acnGetFor dbCn cpId mbrId acctId = do
  (quickQuery' dbCn "\
    \select * \
    \from \
    \  (select acnType, \
    \          amount, \
    \          performedOn, \
    \          (resultOf).prdStart, \
    \          (resultOf).prdType, \
    \          cpId, \
    \          mbrId, \
    \          acctId \
    \  from MemberEquityAction \
    \  union all \
    \  select 'AllocatePatronageRebate', \
    \         f.surplus * allocatedRatio as amount, \
    \         alcPerformedOn, \
    \         (a.resultOf).prdStart, \
    \         (a.resultOf).prdType, \
    \         aa.cpId, \
    \         aa.mbrId, \
    \         aa.acctId \
    \   from Allocation a \
    \     inner join FinancialResults f \
    \        on (a.cpId, a.resultOf) = (f.cpId, f.rsltOver) \
    \     inner join MemberAllocateAction aa \
    \        on (aa.cpId, aa.resultOf) = (a.cpId, a.resultOf) \
    \  union all \
    \  select 'DistributeInstallment', \
    \         f.surplus * allocatedRatio * -dsbProportion as amount, \
    \         dsbPerformedOn, \
    \         (a.resultOf).prdStart, \
    \         (a.resultOf).prdType, \
    \         a.cpId, \
    \         a.mbrId, \
    \         a.acctId \
    \  from MemberAllocateAction a \
    \    inner join FinancialResults f \
    \       on (a.cpId, a.resultOf) = (f.cpId, f.rsltOver) \
    \    inner join Disbursal d \
    \       on (a.cpId, a.resultOf) = (d.cpId, d.resultOf)) act \ 
    \where (cpId,mbrId,acctId) \
    \      = (?,?,?) \
    \order by performedOn asc"
    [toSql cpId, toSql mbrId, toSql acctId]) >>= 
    return . 
      fmap 
        (\r -> 
          (acnFrom r, 
           if (r !! 3) == SqlNull then Nothing else Just $ prdFrom $ drop 3 r))
  
acnFromDsb :: [SqlValue] -> MemberEquityAction
acnFromDsb (performedOn:_:amount:_) = 
  MemberEquityAction{actionType=DistributeInstallment, amount=fromSql amount, 
                     performedOn=fromSql performedOn}

acnGetForDisbursal :: 
  Connection -> Integer -> FiscalPeriod -> Day -> IO [(Member, MemberEquityAction)]
acnGetForDisbursal dbCn cpId resultOf performedOn = do 
  let (prdStartDay, prdType) = prdToSql resultOf
  (quickQuery' dbCn "\
     \select d.dsbPerformedOn, \
     \       fr.surplus * aa.allocatedRatio * -d.dsbProportion as amount, \
     \       m.mbrId, \
     \       m.firstName, \
     \       m.lastName, \
     \       m.acceptedOn \
     \from Disbursal d \
     \ inner join MemberAllocateAction aa \
     \    using (cpId,resultOf) \
     \ inner join FinancialResults fr \
     \    on (aa.resultOf, aa.cpId) = (fr.rsltOver, fr.cpId) \
     \ inner join Member m \
     \    on (aa.cpId,aa.mbrId) = (m.cpId,m.mbrId) \
     \where (d.cpId, (d.resultOf).prdStart, (d.resultOf).prdType, d.dsbPerformedOn) \
     \      = (?,?,?,?)"
     [toSql cpId, prdStartDay, prdType, toSql performedOn]) >>= 
    return . 
      fmap
        (\r -> 
          (mbrFromRow $ drop 4 r, acnFromDsb r))
  
acnExportFor :: Connection -> Integer -> String -> IO () 
acnExportFor dbCn cpId file = 
  void $ 
    run dbCn ("COPY (select * from Member m inner join MemberEquityAccount a using (cpId, mbrId) inner join MemberEquityAction c using (cpId,mbrId,acctId) where cpId = " ++ (show cpId) ++ " order by mbrId, acctId, performedOn) TO '" ++ file ++ "' WITH (FORMAT csv, HEADER)") []
