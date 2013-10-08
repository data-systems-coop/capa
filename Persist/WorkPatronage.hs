module Persist.WorkPatronage
where
  
import Persist.Base  

import System.Log.Logger as LG

import qualified Data.Map as M
import qualified Data.Maybe as MB

import qualified Database.HDBC.PostgreSQL as PG 
import qualified Database.HDBC as DB

import Persist.Member

ptrngGetFor 
  :: PG.Connection -> Integer -> FiscalPeriod -> IO (M.Map Member (Maybe WorkPatronage))
ptrngGetFor dbCn cpId performedOver = do 
  let FiscalPeriod{start=GregorianMonth yr mo, periodType=prdType} = performedOver
  let prdStartDay = fromGregorian yr mo 1
  res <- DB.quickQuery' dbCn "\
    \select m.mbrId, \
    \       m.firstName, \
    \       m.lastName, \
    \       m.acceptedOn, \
    \       p.work, \
    \       p.skillWeightedWork, \
    \       p.quality, \
    \       p.revenueGenerated, \
    \       p.performedOver \
    \from Member m \
    \ left outer join \ 
    \ (select * \ 
    \  from WorkPatronage \
    \  where (performedOver).prdStart = ? \
    \   and (performedOver).prdType = ?) p \
    \ using (cpId,mbrId) \
    \where cpId = ?"
    [DB.SqlLocalDate prdStartDay, DB.SqlString $ show prdType, DB.SqlInteger cpId]
  let ms = fmap mbrFromRow res
  let ps = 
        fmap 
         (\row -> 
            let prow = drop 4 row
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
  DB.run dbCn "\
    \insert into WorkPatronage \
    \values(\
    \  ?,\
    \  ?,\
    \  (?,?),\
    \  ?,\
    \  ?,\
    \  ?,\
    \  ?)"
    [DB.SqlInteger cpId, DB.SqlInteger mbrId, DB.SqlLocalDate prdStartDay, 
     DB.SqlString $ show prdType, DB.SqlInteger wrk, DB.SqlInteger swrk, 
     DB.SqlInteger ql, DB.SqlInteger rvg]
  DB.commit dbCn  
  
ptrngExportFor :: PG.Connection -> Integer -> String -> IO () 
ptrngExportFor dbCn cpId file = 
  void $ 
    DB.run dbCn ("COPY (select * from Member m inner join WorkPatronage p using (cpId, mbrId) where cpId = " ++ (show cpId) ++ " order by mbrId, (performedOver).prdStart) TO '" ++ file ++ "' WITH (FORMAT csv, HEADER)") []



ptrngFromRow :: FiscalPeriod -> [DB.SqlValue] -> WorkPatronage
ptrngFromRow performedOver (work:skillWeightedWork:quality:revenueGenerated:_) = 
  WorkPatronage 
    (DB.fromSql work)
    (DB.fromSql skillWeightedWork)
    0
    (DB.fromSql quality)
    (DB.fromSql revenueGenerated)
    performedOver
