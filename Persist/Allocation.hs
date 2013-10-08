module Persist.Allocation
where
  
import Persist.Base  

import System.Log.Logger as LG

import qualified Data.Map as M
import qualified Data.Maybe as MB

import qualified Database.HDBC.PostgreSQL as PG 
import qualified Database.HDBC as DB

allocFromRow :: [DB.SqlValue] -> Allocation
allocFromRow (performedOn:_) = 
  Allocation{alcPerformedOn=DB.fromSql performedOn}

allocGet :: PG.Connection -> Integer -> FiscalPeriod -> IO Allocation
allocGet dbCn cpId resultOf = do
  let (prdStartDay, prdType) = prdToSql resultOf
  [alloc] <- 
    DB.quickQuery' dbCn "\
      \select alcPerformedOn \
      \from Allocation \
      \where (cpId, (resultOf).prdStart, (resultOf).prdType) \
      \       = (?,?,?)" 
      [DB.SqlInteger cpId, prdStartDay, prdType]
  return $ allocFromRow alloc
  
allocSave :: PG.Connection -> Integer -> FiscalPeriod -> Day -> IO ()
allocSave dbCn cpId over allocatedOn = do 
  let FiscalPeriod{start=GregorianMonth yr mo,periodType=prdType} = over
  let prdStartDay = fromGregorian yr mo 1
  DB.run dbCn "\
      \insert into Allocation \
      \values(\
      \  ?,\
      \  (?,?),\
      \  ?)"
     [DB.SqlInteger cpId, DB.SqlLocalDate prdStartDay, 
      DB.SqlString $ show prdType, DB.SqlLocalDate allocatedOn]
  DB.commit dbCn
