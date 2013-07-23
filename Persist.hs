{-# Language DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Persist 
where
  
import Types
import Utils
import Data.Time (fromGregorian, toGregorian)

import Data.Data            ( Data, Typeable ) 
import qualified Data.Map as M
import Control.Monad.Reader ( ask )      
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update, makeAcidic )
import Data.SafeCopy        ( base, deriveSafeCopy )

import qualified Database.HDBC.PostgreSQL as PG 
import qualified Database.HDBC as DB

type PersistConnection = AcidState Globals

--generally prefer sets not lists
data Globals = Globals { 
  cooperative :: Cooperative,
  settings :: Maybe (AllocationMethod, PatronageWeights, DisbursalSchedule),
  patronage :: M.Map Member [WorkPatronage],
  accounts :: M.Map Member (M.Map MemberEquityAccount [MemberEquityAction]),
  financialResults :: [FinancialResults],
  sessions :: M.Map SessionID (OpenID, Integer)
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
$(deriveSafeCopy 0 'base ''AllocationMethod)
  
putIt :: Globals -> Update Globals Globals
putIt g = 
  do put g
     return g

getIt :: Query Globals Globals
getIt = 
  ask

$(makeAcidic ''Globals ['putIt, 'getIt])

g0 = 
  Globals coop1 settings1 memPatronage1 
          (M.fromList [(m1, M.singleton acct1 []), 
                       (m2, M.singleton acct1 []),
                       (m3, M.singleton acct1 [])])
          res1
          M.empty
-- prdFromRow       

rsltGetFor :: PG.Connection -> Integer -> IO [FinancialResults]
rsltGetFor dbCn cpId = do
  DB.quickQuery dbCn 
    "select (rsltOver).prdStart, (rsltOver).prdType, surplus, allocatedOn from FinancialResults where cpId = ?" [DB.SqlInteger cpId]
  >>= mapM (return . rsltFromRow)

rsltFromRow :: [DB.SqlValue] -> FinancialResults  --private
rsltFromRow (rsltOverStart:rsltOverType:surplus:allocatedOn:_) = 
  let (yr,mo,_) = toGregorian $ DB.fromSql rsltOverStart
      prdType = read $ DB.fromSql rsltOverType
  in FinancialResults 
      (FiscalPeriod (GregorianMonth yr mo) prdType)
      (DB.fromSql surplus)
      (DB.fromSql allocatedOn)
      
rsltSaveFor :: PG.Connection -> Integer -> FinancialResults -> IO ()
rsltSaveFor dbCn cpId FinancialResults{over=over,surplus=srpls,allocatedOn=Nothing} = do 
  let FiscalPeriod{start=GregorianMonth yr mo,periodType=prdType} = over
  let prdStartDay = fromGregorian yr mo 1
  DB.run dbCn 
    "insert into FinancialResults values(?,(?,?),?)"
    [DB.SqlInteger cpId, DB.SqlLocalDate prdStartDay, DB.SqlString $ show prdType,
     DB.SqlInteger srpls]
  DB.commit dbCn  
  
-- rsltSaveAllocated :: PG.Connection -> Integer -> FinancialResults -> IO ()  

-- save alloc settings

ptrngGetFor 
  :: PG.Connection -> Integer -> FiscalPeriod -> IO (M.Map Member (Maybe WorkPatronage))
ptrngGetFor dbCn cpId performedOver = do 
  res <- DB.quickQuery dbCn
    "select m.mbrId, m.firstName, p.work, p.skillWeightedWork, p.quality, p.revenueGenerated, p.performedOver from Member m left outer join WorkPatronage p using (cpId,mbrId) where cpId = cpId and performedOver = performedOver"
    []
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
                revenueGenerated=rvg,performedOver=prf}= do
  let FiscalPeriod{start=GregorianMonth yr mo,periodType=prdType} = prf
  let prdStartDay = fromGregorian yr mo 1
  DB.run dbCn 
    "insert into WorkPatronage values(?,?,(?,?),?,?,?,?)"
    [DB.SqlInteger cpId, DB.SqlInteger mbrId, DB.SqlLocalDate prdStartDay, 
     DB.SqlString $ show prdType, DB.SqlInteger wrk, DB.SqlInteger swrk, 
     DB.SqlInteger ql, DB.SqlInteger rvg]
  DB.commit dbCn  
  
mbrFromRow :: [DB.SqlValue] -> Member
mbrFromRow (mbrId:firstName:_) = 
  Member (DB.fromSql firstName) (DB.fromSql mbrId)
  
mbrGetFor :: PG.Connection -> Integer -> IO [Member]
mbrGetFor dbCn cpId = do
  res <- DB.quickQuery dbCn "select mbrId,firstName from member where cpId=cpId" []
  return $ fmap mbrFromRow res

ptrngFromRow :: FiscalPeriod -> [DB.SqlValue] -> WorkPatronage
ptrngFromRow performedOver (work:skillWeightedWork:quality:revenueGenerated:_) = 
  WorkPatronage 
    (DB.fromSql work)
    (DB.fromSql skillWeightedWork)
    0
    (DB.fromSql quality)
    (DB.fromSql revenueGenerated)
    performedOver
    
acnSaveFor :: 
  PG.Connection -> Integer -> Integer -> Integer -> FiscalPeriod -> MemberEquityAction -> IO ()
acnSaveFor 
  dbCn cpId mbrId acctId resultOf
  MemberEquityAction{actionType=tp,amount=amt,performedOn=prf} = do 
    let FiscalPeriod{start=GregorianMonth yr mo,periodType=prdType} = resultOf
    let prdStartDay = fromGregorian yr mo 1
    DB.run dbCn 
      "insert into MemberEquityAction values(?,?,?,?,?,?,(?,?))"
      [DB.SqlInteger cpId, DB.SqlInteger mbrId, DB.SqlInteger acctId, 
       DB.SqlString $ show tp, DB.SqlInteger amt, DB.SqlLocalDate prf,
       DB.SqlLocalDate prdStartDay, DB.SqlString $ show prdType]
    DB.commit dbCn
    
allocStngGetFor :: PG.Connection -> Integer -> IO (AllocationMethod, PatronageWeights) 
allocStngGetFor dbCn cpId = do 
  (res:_) <- DB.quickQuery dbCn "select * from CoopSettings where cpId = cpId" []
  return 
   (read $ DB.fromSql $ res !! 0,
    PatronageWeights 
     (DB.fromSql $ res !! 1)
     (DB.fromSql $ res !! 2)
     (DB.fromSql $ res !! 3)
     (DB.fromSql $ res !! 4)
     (DB.fromSql $ res !! 5))
   
dsbSchedGetFor :: PG.Connection -> Integer -> IO DisbursalSchedule   
dsbSchedGetFor dbCn cpId = do
  res <- DB.quickQuery dbCn "select (afterAllocation).years, (afterAllocation).months, proportion from DisbursalSchedule where cpId = cpId" []
  return $ 
    fmap 
      (\(yr:mo:prop:_) -> 
        (GregorianDuration (DB.fromSql yr) (DB.fromSql mo), DB.fromSql prop))
      res
        