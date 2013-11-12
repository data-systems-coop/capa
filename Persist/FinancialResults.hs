module Persist.FinancialResults
where
  
import Persist.Base  

import System.Log.Logger as LG

import qualified Data.Map as M
import qualified Data.Maybe as MB

import qualified Database.HDBC.PostgreSQL as PG 
import qualified Database.HDBC as DB

rsltGetAll :: PG.Connection -> Integer -> IO [FinancialResults]
rsltGetAll dbCn cpId = do
  DB.quickQuery' dbCn "\
    \select (rsltOver).prdStart, \
    \       (rsltOver).prdType, \
    \       surplus, \
    \       (select alcPerformedOn \
    \        from Allocation \
    \        where (cpId, resultOf) = (f.cpId, rsltOver)) \
    \from FinancialResults f \
    \where cpId = ?" [DB.SqlInteger cpId]
  >>= mapM (return . rsltFromRow)

rsltGetForOver :: 
  PG.Connection -> Integer -> FiscalPeriod -> IO (Maybe FinancialResults)
rsltGetForOver dbCn cpId rsltOver = do 
  let FiscalPeriod{start=GregorianMonth yr mo, periodType=prdType} = rsltOver
  let prdStartDay = fromGregorian yr mo 1
  rows <- DB.quickQuery' dbCn "\
    \select (rsltOver).prdStart, \
    \        (rsltOver).prdType, \
    \        surplus, \
    \        null \
    \from FinancialResults \
    \where cpId = ? \
    \ and (rsltOver).prdStart = ? \
    \ and (rsltOver).prdType = ?"
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
rsltSaveFor dbCn cpId FinancialResults{over=over,surplus=srpls} = 
  do 
    let FiscalPeriod{start=GregorianMonth yr mo,periodType=prdType} = over
    let prdStartDay = fromGregorian yr mo 1
    DB.run dbCn "\
      \insert into FinancialResults \
      \values( \
      \    ?,\
      \    (?,?),\
      \     ?)"
      [DB.SqlInteger cpId, DB.SqlLocalDate prdStartDay, DB.SqlString $ show prdType,
       DB.SqlInteger srpls]
    DB.commit dbCn  
  
rsltDelete :: PG.Connection -> Integer -> FiscalPeriod -> IO ()
rsltDelete dbCn cpId over = do 
  let FiscalPeriod{start=GregorianMonth yr mo,periodType=prdType} = over
  let prdStartDay = fromGregorian yr mo 1
  DB.run dbCn "\
      \delete from FinancialResults \
      \where (cpId, (rsltOver).prdStart, (rsltOver).prdType) = (?,?,?)"
      [DB.SqlInteger cpId, DB.SqlLocalDate prdStartDay, DB.SqlString $ show prdType]
  DB.commit dbCn  
