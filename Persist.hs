{-# Language DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Persist 
where
  
import Types
import Utils
import Data.Time (fromGregorian, toGregorian, Day(..))

import Data.Data            ( Data, Typeable ) 
import qualified Data.Map as M
import Control.Monad.Reader ( ask )      
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update, makeAcidic )
import Data.SafeCopy        ( base, deriveSafeCopy )

import qualified Database.HDBC.PostgreSQL as PG 
import qualified Database.HDBC as DB

import qualified Data.Maybe as MB
import Control.Monad ( void )

type PersistConnection = AcidState Globals

data Globals = Globals { 
  sessions :: M.Map SessionID (OpenID, Integer) -- add alloc method?
} deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Globals)
  
putIt :: Globals -> Update Globals Globals
putIt g = put g >> return g

getIt :: Query Globals Globals
getIt = ask

$(makeAcidic ''Globals ['putIt, 'getIt])

g0 = Globals M.empty
--generally prefer sets not lists

coopFromRow :: [DB.SqlValue] -> Cooperative
coopFromRow [cpId,name,username,usageStart,usageEnd,calStart,calPrd] = 
  let calType = FiscalCalendarType (DB.fromSql calStart) (read $ DB.fromSql calPrd)
  in Cooperative (DB.fromSql cpId) (DB.fromSql name) (DB.fromSql username)
    (DB.fromSql usageStart) (DB.fromSql usageEnd) calType

coopGet :: PG.Connection -> Integer -> IO Cooperative
coopGet dbCn cpId = do 
  (row:_) <- DB.quickQuery dbCn 
    "select cpId, cpName, username, usageStart, usageEnd, (fiscalCalendarType).start, (fiscalCalendarType).prdType from Cooperative where cpId = ?" [DB.toSql cpId]
  return $ coopFromRow row

rsltGetAll :: PG.Connection -> Integer -> IO [FinancialResults]
rsltGetAll dbCn cpId = do
  DB.quickQuery dbCn 
    "select (rsltOver).prdStart, (rsltOver).prdType, surplus, allocatedOn from FinancialResults where cpId = ?" [DB.SqlInteger cpId]
  >>= mapM (return . rsltFromRow)

rsltGetForOver :: 
  PG.Connection -> Integer -> FiscalPeriod -> IO (Maybe FinancialResults)
rsltGetForOver dbCn cpId rsltOver = do 
  let FiscalPeriod{start=GregorianMonth yr mo, periodType=prdType} = rsltOver
  let prdStartDay = fromGregorian yr mo 1
  rows <- DB.quickQuery dbCn
    "select (rsltOver).prdStart, (rsltOver).prdType, surplus, allocatedOn from FinancialResults where cpId = ? and (rsltOver).prdStart = ? and (rsltOver).prdType = ?"
    [DB.SqlInteger cpId, DB.SqlLocalDate prdStartDay, DB.SqlString $ show prdType]
  let mb = MB.listToMaybe rows
  return $ fmap rsltFromRow mb 

rsltFromRow :: [DB.SqlValue] -> FinancialResults  --private
rsltFromRow (rsltOverStart:rsltOverType:surplus:allocatedOn:_) = 
  let (yr,mo,_) = toGregorian $ DB.fromSql rsltOverStart
      prdType = read $ DB.fromSql rsltOverType
  in FinancialResults 
      (FiscalPeriod (GregorianMonth yr mo) prdType)
      (DB.fromSql surplus)
      (DB.fromSql allocatedOn)
      
rsltSaveFor :: PG.Connection -> Integer -> FinancialResults -> IO ()
rsltSaveFor dbCn cpId FinancialResults{over=over,surplus=srpls,allocatedOn=Nothing} = 
  do 
    let FiscalPeriod{start=GregorianMonth yr mo,periodType=prdType} = over
    let prdStartDay = fromGregorian yr mo 1
    DB.run dbCn 
      "insert into FinancialResults values(?,(?,?),?)"
      [DB.SqlInteger cpId, DB.SqlLocalDate prdStartDay, DB.SqlString $ show prdType,
       DB.SqlInteger srpls]
    DB.commit dbCn  
  
rsltUpdateAllocated 
  :: PG.Connection -> Integer -> FiscalPeriod -> Day -> IO ()
rsltUpdateAllocated dbCn cpId over allocatedOn = do 
  do 
    let FiscalPeriod{start=GregorianMonth yr mo,periodType=prdType} = over
    let prdStartDay = fromGregorian yr mo 1
    DB.run dbCn
       "update FinancialResults set allocatedOn = ? where cpId = ? and (rsltOver).prdStart = ? and (rsltOver).prdType = ?"
       [DB.SqlLocalDate allocatedOn, DB.SqlInteger cpId, DB.SqlLocalDate prdStartDay, 
        DB.SqlString $ show prdType]
    DB.commit dbCn

ptrngGetFor 
  :: PG.Connection -> Integer -> FiscalPeriod -> IO (M.Map Member (Maybe WorkPatronage))
ptrngGetFor dbCn cpId performedOver = do 
  let FiscalPeriod{start=GregorianMonth yr mo, periodType=prdType} = performedOver
  let prdStartDay = fromGregorian yr mo 1
  res <- DB.quickQuery dbCn
    "select m.mbrId, m.firstName, p.work, p.skillWeightedWork, p.quality, p.revenueGenerated, p.performedOver from Member m left outer join (select * from WorkPatronage where (performedOver).prdStart = ? and (performedOver).prdType = ?) p using (cpId,mbrId) where cpId = ?"
    [DB.SqlLocalDate prdStartDay, DB.SqlString $ show prdType, DB.SqlInteger cpId]
  let ms = fmap mbrFromRow res
  let ps = 
        fmap 
         (\row -> 
            let prow = drop 2 row
            in if (head prow) == DB.SqlNull
               then Nothing
               else Just $ ptrngFromRow performedOver prow)
         res
  return (M.fromList $ zip ms ps)
  
ptrngSaveFor :: PG.Connection -> Integer -> Integer -> WorkPatronage -> IO ()
ptrngSaveFor dbCn cpId mbrId 
  WorkPatronage{work=wrk,skillWeightedWork=swrk,quality=ql, 
                revenueGenerated=rvg,performedOver=prf} = do
  let FiscalPeriod{start=GregorianMonth yr mo,periodType=prdType} = prf
  let prdStartDay = fromGregorian yr mo 1
  DB.run dbCn 
    "insert into WorkPatronage values(?,?,(?,?),?,?,?,?)"
    [DB.SqlInteger cpId, DB.SqlInteger mbrId, DB.SqlLocalDate prdStartDay, 
     DB.SqlString $ show prdType, DB.SqlInteger wrk, DB.SqlInteger swrk, 
     DB.SqlInteger ql, DB.SqlInteger rvg]
  DB.commit dbCn  
  
ptrngExportFor :: PG.Connection -> Integer -> String -> IO () 
ptrngExportFor dbCn cpId file = 
  void $ 
    DB.run dbCn ("COPY (select * from Member m inner join WorkPatronage p using (cpId, mbrId) where cpId = " ++ (show cpId) ++ " order by mbrId, (performedOver).prdStart) TO '" ++ file ++ "' WITH (FORMAT csv, HEADER)") []


mbrFromRow :: [DB.SqlValue] -> Member
mbrFromRow (mbrId:firstName:_) = 
  Member (DB.fromSql firstName) (DB.fromSql mbrId)
  
mbrGetAll :: PG.Connection -> Integer -> IO [(Member,Money)]
mbrGetAll dbCn cpId = do
  (DB.quickQuery dbCn "select mbrId,firstName, coalesce((select sum(amount) from MemberEquityAction where (cpId,mbrId) = (a.cpId,a.mbrId)),0) as total from member a where cpId=?"
     [DB.SqlInteger cpId]) >>= 
    return . fmap (\r -> (mbrFromRow r,DB.fromSql $ r !! 2))

mbrGet :: PG.Connection -> Integer -> Integer -> IO (Maybe Member)
mbrGet dbCn cpId mbrId = 
  DB.quickQuery dbCn "select mbrId,firstName from member where (cpId,mbrId)=(?,?)"
    [DB.SqlInteger cpId, DB.SqlInteger mbrId] >>= 
  return . fmap mbrFromRow . MB.listToMaybe

ptrngFromRow :: FiscalPeriod -> [DB.SqlValue] -> WorkPatronage
ptrngFromRow performedOver (work:skillWeightedWork:quality:revenueGenerated:_) = 
  WorkPatronage 
    (DB.fromSql work)
    (DB.fromSql skillWeightedWork)
    0
    (DB.fromSql quality)
    (DB.fromSql revenueGenerated)
    performedOver

acctFrom :: [DB.SqlValue] -> MemberEquityAccount
acctFrom (acctId:acctType:_) = 
  MemberEquityAccount (DB.fromSql acctId) (read $ DB.fromSql acctType)

acctGet :: PG.Connection -> Integer -> Integer -> Integer -> IO MemberEquityAccount
acctGet dbCn cpId mbrId acctId = 
  (DB.quickQuery dbCn "select acctId, acctType from MemberEquityAccount where (cpId,mbrId,acctId) = (?,?,?)" $ fmap DB.toSql [cpId, mbrId, acctId]) >>= 
  return . acctFrom . head 

acctGetAll :: PG.Connection -> Integer -> IO (M.Map Member [MemberEquityAccount])
acctGetAll dbCn cpId = 
  DB.quickQuery dbCn "select m.mbrId, m.firstName, acctId, acctType from Member m inner join MemberEquityAccount a using (mbrId,cpId) where m.cpId = ?" [DB.toSql cpId] >>=
  return . M.fromListWith (++) . fmap (\r -> (mbrFromRow r, [acctFrom (drop 2 r)])) 
  
prdToSql :: FiscalPeriod -> (DB.SqlValue, DB.SqlValue)
prdToSql FiscalPeriod{start=GregorianMonth yr mo, periodType = prdType} = 
  (DB.toSql $ fromGregorian yr mo 1, DB.toSql $ show prdType)

acnSaveFor :: 
  PG.Connection -> Integer -> Integer -> Integer -> Maybe FiscalPeriod 
  -> MemberEquityAction -> IO ()
acnSaveFor 
  dbCn cpId mbrId acctId resultOf 
  MemberEquityAction{actionType=tp,amount=amt,performedOn=prf} = do 
    let (prdStartDay, prdType) = maybe (DB.SqlNull, DB.SqlNull) prdToSql resultOf
    DB.run dbCn 
      "insert into MemberEquityAction values(?,?,?,?,?,?,(?,?))"
      [DB.SqlInteger cpId, DB.SqlInteger mbrId, DB.SqlInteger acctId, 
       DB.SqlString $ show tp, DB.SqlInteger amt, DB.SqlLocalDate prf,
       prdStartDay, prdType]
    DB.commit dbCn
    
acnSaveToRolling :: 
  PG.Connection -> Integer -> Integer -> FiscalPeriod -> MemberEquityAction
  -> IO ()
acnSaveToRolling dbCn cpId mbrId resultOf acn = do
    ((acctId:_):_) <- 
      DB.quickQuery dbCn "select acctId from MemberEquityAccount where (cpId,mbrId,acctType) = (?,?,?)" 
        [DB.SqlInteger cpId, DB.SqlInteger mbrId, DB.SqlString $ show RollingPatronage]
    let rollingAcctId = DB.fromSql acctId
    acnSaveFor dbCn cpId mbrId rollingAcctId (Just resultOf) acn
    
acnFrom :: [DB.SqlValue] -> MemberEquityAction 
acnFrom (acnType:amnt:perfOn:_) = 
  MemberEquityAction (read $ DB.fromSql acnType) (DB.fromSql amnt) (DB.fromSql perfOn)

prdFrom :: [DB.SqlValue] -> FiscalPeriod --use more
prdFrom (start:prdType:_) = 
  let (yr,mo,_) = toGregorian $ DB.fromSql start
  in FiscalPeriod (GregorianMonth yr mo) (read $ DB.fromSql prdType)

acnGetFor :: 
  PG.Connection -> Integer -> Integer -> Integer -> 
    IO [(MemberEquityAction, Maybe FiscalPeriod)]
acnGetFor dbCn cpId mbrId acctId = do
  (DB.quickQuery dbCn "select acnType, amount, performedOn, (resultOf).prdStart, (resultOf).prdType from MemberEquityAction where (cpId,mbrId,acctId) = (?,?,?) order by performedOn asc"
    [DB.toSql cpId, DB.toSql mbrId, DB.toSql acctId]) >>= 
    return . 
      fmap 
        (\r -> 
          (acnFrom r, 
           if (r !! 3) == DB.SqlNull then Nothing else Just $ prdFrom $ drop 3 r))
  
acnExportFor :: PG.Connection -> Integer -> String -> IO () 
acnExportFor dbCn cpId file = 
  void $ 
    DB.run dbCn ("COPY (select * from Member m inner join MemberEquityAccount a using (cpId, mbrId) inner join MemberEquityAction c using (cpId,mbrId,acctId) where cpId = " ++ (show cpId) ++ " order by mbrId, acctId, performedOn) TO '" ++ file ++ "' WITH (FORMAT csv, HEADER)") []

allocStngGet :: PG.Connection -> Integer -> IO (AllocationMethod, PatronageWeights) 
allocStngGet dbCn cpId = do 
  (res:_) <- 
    DB.quickQuery dbCn 
      "select allocationMethod, work, skillWeightedWork, seniority, quality, revenueGenerated from CoopSettings where cpId = ?" 
      [DB.SqlInteger cpId]
  return 
   (read $ DB.fromSql $ res !! 0,
    PatronageWeights 
     (DB.fromSql $ res !! 1)
     (DB.fromSql $ res !! 2)
     (DB.fromSql $ res !! 3)
     (DB.fromSql $ res !! 4)
     (DB.fromSql $ res !! 5))
   
toSqlDouble :: Rational -> DB.SqlValue
toSqlDouble = DB.toSql . (fromRational::Rational -> Double)

allocStngSaveFor :: 
  PG.Connection -> Integer -> AllocationMethod -> PatronageWeights -> IO ()
allocStngSaveFor dbCn cpId allocMethod 
  PatronageWeights{workw=workw,skillWeightedWorkw=skillWeightedWorkw,
                   seniorityw=seniorityw,qualityw=qualityw,
                   revenueGeneratedw=revenueGeneratedw}= do
  DB.run dbCn
    "insert into CoopSettings values (?,?,?,?,?,?,?)"
    [DB.toSql cpId, DB.toSql $ show allocMethod, 
     toSqlDouble workw, toSqlDouble skillWeightedWorkw, toSqlDouble seniorityw,
     toSqlDouble qualityw, toSqlDouble revenueGeneratedw]
  DB.commit dbCn

snrtyMpngsSaveFor :: 
  PG.Connection -> Integer -> SeniorityMappings -> IO ()
snrtyMpngsSaveFor dbCn cpId mpngs = do 
  mapM_ 
    (\(ent,lvl) -> 
      DB.run dbCn "insert into SeniorityMappings values (?,?,?)" 
        [DB.toSql cpId, DB.toSql $ snrtyMpEntStart ent, DB.toSql lvl])
    (M.toList mpngs)
  DB.commit dbCn

snrtyMpngsGet :: PG.Connection -> Integer -> IO SeniorityMappings
snrtyMpngsGet dbCn cpId =
  (DB.quickQuery dbCn "select startYear, snrtyMpngLevel from SeniorityMappings where cpId = ? order by startYear asc" [DB.toSql cpId]) >>= 
    return . M.fromList . fmap snrtyMpngFromRow
  
snrtyMpngFromRow :: [DB.SqlValue] -> (SeniorityMappingEntry, SeniorityLevel)
snrtyMpngFromRow [startYear,level] = 
  (SeniorityMappingEntry $ DB.fromSql startYear, DB.fromSql level)

dsbSchedGet :: PG.Connection -> Integer -> IO DisbursalSchedule   
dsbSchedGet dbCn cpId = do
  res <- DB.quickQuery dbCn "select (afterAllocation).years, (afterAllocation).months, proportion from DisbursalSchedule where cpId = ?" [DB.SqlInteger cpId]
  return $ 
    fmap 
      (\(yr:mo:prop:_) -> 
        (GregorianDuration (DB.fromSql yr) (DB.fromSql mo), DB.fromSql prop))
      res

dsbSchedSaveFor :: PG.Connection -> Integer -> DisbursalSchedule -> IO ()
dsbSchedSaveFor dbCn cpId schd = do
  mapM_
    (\(GregorianDuration yr mo, portion) -> 
      DB.run dbCn "insert into DisbursalSchedule values (?,(?,?),?)" 
        [DB.toSql cpId, DB.toSql yr, DB.toSql mo, toSqlDouble portion])
    schd
  DB.commit dbCn