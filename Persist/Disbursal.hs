module Persist.Disbursal
where
  
import Persist.Base  

import qualified Data.Map as M

disbFromRow :: [SqlValue] -> Disbursal
disbFromRow (performedOn:proportion:_) = 
  Disbursal {dsbPerformedOn=fromSql performedOn, dsbProportion=fromSql proportion}

disbursalGetFor :: Connection -> Integer -> FiscalPeriod -> IO [Disbursal]
disbursalGetFor dbCn cpId resultOf = do
  let (prdStartDay, prdType) = prdToSql resultOf
  (quickQuery' dbCn "\
        \select dsbPerformedOn, \
        \       dsbProportion \
        \from Disbursal \
        \where (cpId, (resultOf).prdStart, (resultOf).prdType) \
        \        = (?,?,?)" 
      [SqlInteger cpId, prdStartDay, prdType]) >>= 
    (return . fmap disbFromRow)

disbursalSave :: Connection -> Integer -> FiscalPeriod -> Disbursal -> IO ()
disbursalSave dbCn cpId resultOf Disbursal{dsbPerformedOn=on,dsbProportion=prop} = do 
  let (prdStartDay, prdType) = prdToSql resultOf
  run dbCn "\
      \insert into Disbursal \
      \values (\
      \  ?,\
      \  (?,?),\
      \  ?,\
      \  ?)"
    [SqlInteger cpId, prdStartDay, prdType, toSql on, toSqlDouble prop]
  commit dbCn

