{-# Language DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Persist.Persist ( 
  module Persist.Persist, 
  module Persist.Cooperative,
  module Persist.MemberEquityAccount,
  module Persist.Member,
  module Persist.FinancialResults,
  module Persist.WorkPatronage,
  module Persist.Allocation,
  module Persist.Disbursal,
  module Persist.MemberAction
)
where
  
import Persist.Base

import Data.Data            ( Data, Typeable ) 
import qualified Data.Map as M
import Control.Monad.Reader ( ask )      
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update, makeAcidic )
import Data.SafeCopy        ( base, deriveSafeCopy )

import qualified Database.HDBC as DB

import Persist.Cooperative
import Persist.MemberEquityAccount
import Persist.Member
import Persist.FinancialResults
import Persist.WorkPatronage
import Persist.Allocation
import Persist.Disbursal
import Persist.MemberAction


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



--AllocationSetting
allocStngGet :: Connection -> Integer -> IO (AllocationMethod, PatronageWeights) 
allocStngGet dbCn cpId = do 
  (res:_) <- 
    DB.quickQuery' dbCn "\
      \select allocationMethod,\ 
      \       work, \
      \       skillWeightedWork, \
      \       seniority, \
      \       quality, \
      \       revenueGenerated \
      \from CoopSettings \
      \where cpId = ?" 
      [DB.SqlInteger cpId]
  return 
   (read $ DB.fromSql $ res !! 0,
    PatronageWeights 
     (DB.fromSql $ res !! 1)
     (DB.fromSql $ res !! 2)
     (DB.fromSql $ res !! 3)
     (DB.fromSql $ res !! 4)
     (DB.fromSql $ res !! 5))
   

allocStngSaveFor :: 
  Connection -> Integer -> AllocationMethod -> PatronageWeights -> IO ()
allocStngSaveFor dbCn cpId allocMethod 
  PatronageWeights{workw=workw,skillWeightedWorkw=skillWeightedWorkw,
                   seniorityw=seniorityw,qualityw=qualityw,
                   revenueGeneratedw=revenueGeneratedw}= do
  DB.run dbCn "\
    \insert into CoopSettings \
    \values (\
    \   ?,\
    \   ?,\
    \   ?,\
    \   ?,\
    \   ?,\
    \   ?,\
    \   ?)"
    [DB.toSql cpId, DB.toSql $ show allocMethod, 
     toSqlDouble workw, toSqlDouble skillWeightedWorkw, toSqlDouble seniorityw,
     toSqlDouble qualityw, toSqlDouble revenueGeneratedw]
  DB.commit dbCn

stngExportFor :: Connection -> Integer -> String -> IO () 
stngExportFor dbCn cpId file = 
  void $ --snrty levls eventually
    DB.run dbCn ("COPY (select *, (select string_agg( (afterAllocation).years || 'yrs' || (afterAllocation).months || 'mos' || ' ' || proportion*100 || '%', ',') from DisbursalSchedule where cpId = " ++ (show cpId) ++ ") as disbursalScheduleList from Cooperative inner join CoopSettings using (cpId) where cpId = " ++ (show cpId) ++ ") TO '" ++ file ++ "' WITH (FORMAT csv, HEADER)") []

--SeniorityMappings
snrtyMpngsSaveFor :: 
  Connection -> Integer -> SeniorityMappings -> IO ()
snrtyMpngsSaveFor dbCn cpId mpngs = do 
  mapM_ 
    (\(ent,lvl) -> 
      DB.run dbCn "\
          \insert into SeniorityMappings\ 
          \values (\
          \  ?,\
          \  ?,\
          \  ?)" 
        [DB.toSql cpId, DB.toSql $ snrtyMpEntStart ent, DB.toSql lvl])
    (M.toList mpngs)
  DB.commit dbCn

snrtyMpngsGet :: Connection -> Integer -> IO SeniorityMappings
snrtyMpngsGet dbCn cpId =
  (DB.quickQuery' dbCn "\
           \select startYear, \
           \       snrtyMpngLevel \
           \from SeniorityMappings \
           \where cpId = ? \
           \order by startYear asc" [DB.toSql cpId]) >>= 
    return . M.fromList . fmap snrtyMpngFromRow
  
snrtyMpngFromRow :: [DB.SqlValue] -> (SeniorityMappingEntry, SeniorityLevel)
snrtyMpngFromRow [startYear,level] = 
  (SeniorityMappingEntry $ DB.fromSql startYear, DB.fromSql level)

--DisbursalSchedule
dsbSchedGet :: Connection -> Integer -> IO DisbursalSchedule   
dsbSchedGet dbCn cpId = do
  res <- DB.quickQuery' dbCn "\
          \select (afterAllocation).years, \
          \       (afterAllocation).months, \
          \       proportion \
          \from DisbursalSchedule \
          \where cpId = ?" [DB.SqlInteger cpId]
  return $ 
    fmap 
      (\(yr:mo:prop:_) -> 
        (GregorianDuration (DB.fromSql yr) (DB.fromSql mo), DB.fromSql prop))
      res

dsbSchedSaveFor :: Connection -> Integer -> DisbursalSchedule -> IO ()
dsbSchedSaveFor dbCn cpId schd = do
  mapM_
    (\(GregorianDuration yr mo, portion) -> 
      DB.run dbCn "\
         \insert into DisbursalSchedule \
         \values (\
         \   ?,\
         \   (?,?),\
         \   ?)" 
        [DB.toSql cpId, DB.toSql yr, DB.toSql mo, toSqlDouble portion])
    schd
  DB.commit dbCn

--base
coopRegisterState :: Connection -> Integer -> IO (Bool, Bool)
coopRegisterState dbCn cpId = do 
  [[alloc,disb]] <- DB.quickQuery' dbCn "\
         \select (select count(*) from CoopSettings where cpId = ?) > 0,\ 
         \       (select count(*) from DisbursalSchedule where cpId = ?) > 0" 
    [DB.toSql cpId, DB.toSql cpId]
  return (DB.fromSql alloc, DB.fromSql disb)