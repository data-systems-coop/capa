{-# Language DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Persist 
where
  
import Types
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
  Globals coop1 settings1 [m1, m2, m3] memPatronage1 
          (M.fromList [(m1, M.singleton acct1 []), 
                       (m2, M.singleton acct1 []),
                       (m3, M.singleton acct1 [])])
          res1 allocs1

rsltGetFor :: PG.Connection -> Integer -> IO [FinancialResults]
rsltGetFor dbCn cpId = do
  DB.quickQuery dbCn 
    "select (rsltOver).prdStart, (rsltOver).prdType, surplus, allocatedOn from FinancialResults where cpId = cpId" []
  >>= mapM (return . rsltFromRow)

rsltFromRow :: [DB.SqlValue] -> FinancialResults  --private
rsltFromRow (rsltOverStart:rsltOverType:surplus:allocatedOn:_) = 
  let (yr,mo,_) = toGregorian $ DB.fromSql rsltOverStart
      prdType = read $ DB.fromSql rsltOverType
  in FinancialResults 
      (FiscalPeriod (GregorianMonth yr mo) prdType)
      (DB.fromSql surplus)
      (DB.fromSql allocatedOn)
      
-- prdFromRow       

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
  
mbrFromRow :: [DB.SqlValue] -> Member
mbrFromRow (mbrId:firstName:_) = 
  Member (DB.fromSql firstName) (DB.fromSql mbrId)
  
ptrngFromRow :: FiscalPeriod -> [DB.SqlValue] -> WorkPatronage
ptrngFromRow performedOver (work:skillWeightedWork:quality:revenueGenerated:_) = 
  WorkPatronage 
    (DB.fromSql work)
    (DB.fromSql skillWeightedWork)
    0
    (DB.fromSql quality)
    (DB.fromSql revenueGenerated)
    performedOver