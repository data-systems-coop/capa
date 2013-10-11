module Persist.Cooperative
where
  
import Persist.Base  

import qualified Data.Map as M

import qualified Database.HDBC.PostgreSQL as PG 
import qualified Database.HDBC as DB

import Control.Monad.Reader

coopFromRow :: [DB.SqlValue] -> Cooperative
coopFromRow [cpId,name,username,usageStart,usageEnd,calStart,calPrd] = 
  let calType = FiscalCalendarType (DB.fromSql calStart) (read $ DB.fromSql calPrd)
  in Cooperative (DB.fromSql cpId) (DB.fromSql name) (DB.fromSql username)
    (DB.fromSql usageStart) (DB.fromSql usageEnd) calType

coopGet :: ReaderT (PG.Connection, Integer) IO Cooperative
coopGet = do 
  (dbCn, cpId) <- ask
  lift $ do 
   (row:_) <- DB.quickQuery' dbCn "\
    \select cpId, \
    \       cpName, \
    \       username, \
    \       usageStart, \
    \       usageEnd, \
    \       (fiscalCalendarType).start, \
    \       (fiscalCalendarType).prdType \
    \from Cooperative \
    \where cpId = ?" [DB.toSql cpId]
   return $ coopFromRow row

coopGetFor :: PG.Connection -> OpenID -> IO (Maybe Cooperative)
coopGetFor dbCn username = do 
  rows <- DB.quickQuery' dbCn "\
    \select cpId, \
    \       cpName, \
    \       username, \
    \       usageStart, \
    \       usageEnd, \
    \       (fiscalCalendarType).start, \
    \       (fiscalCalendarType).prdType \
    \from Cooperative \
    \where username = ?" [DB.toSql username]
  return $ fmap coopFromRow $ listToMaybe rows

coopSave :: Cooperative -> ReaderT PG.Connection IO Integer
coopSave Cooperative{name=nm,username=usr,usageStart=st,fiscalCalendarType=clTp} = do
  dbCn <- ask
  lift $ do 
   let FiscalCalendarType{startf=fst,periodTypef=typ} = clTp
   DB.run dbCn "\
     \insert into Cooperative(\
     \              cpid, \
     \              cpName, \
     \              username, \
     \              usageStart, \
     \              fiscalCalendarType) \
     \values (\
     \        (select coalesce(max(cpId),0)+1 from Cooperative),\
     \        ?,\
     \        ?,\
     \        ?,\
     \        (?,?))" 
        [DB.toSql nm, DB.toSql usr, DB.toSql st, DB.toSql fst, DB.toSql $ show typ] 
   [[cpId]] <- DB.quickQuery' dbCn "select max(cpId) from Cooperative" []
   DB.commit dbCn
   return $ DB.fromSql cpId
