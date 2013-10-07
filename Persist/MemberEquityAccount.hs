{-# Language DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Persist.MemberEquityAccount
where
  
import Types
import Utils
import System.Log.Logger as LG
import Text.Printf(printf)
import Data.Time (fromGregorian, toGregorian, Day(..))

import qualified Data.Map as M

import qualified Database.HDBC.PostgreSQL as PG 
import qualified Database.HDBC as DB

import qualified Data.Maybe as MB
import Control.Monad ( void )

import Persist.Member

acctFrom :: [DB.SqlValue] -> MemberEquityAccount
acctFrom (acctId:acctType:_) = 
  MemberEquityAccount (DB.fromSql acctId) (read $ DB.fromSql acctType)

acctGet :: PG.Connection -> Integer -> Integer -> Integer -> IO MemberEquityAccount
acctGet dbCn cpId mbrId acctId = 
  (DB.quickQuery' dbCn "\
         \select acctId, \
         \       acctType \
         \from MemberEquityAccount \
         \where (cpId,mbrId,acctId) = (?,?,?)" $ 
      fmap DB.toSql [cpId, mbrId, acctId]) >>= 
  return . acctFrom . head 

acctGetAll :: PG.Connection -> Integer -> IO (M.Map Member [MemberEquityAccount])
acctGetAll dbCn cpId = 
  DB.quickQuery' dbCn "\
        \select m.mbrId, \
        \       m.firstName, \
        \       m.lastName, \
        \       m.acceptedOn, \
        \       acctId, \ 
        \       acctType \
        \from Member m \
        \inner join MemberEquityAccount a using (mbrId,cpId) \
        \where m.cpId = ?" [DB.toSql cpId] >>= 
    return . M.fromListWith (++) . fmap (\r -> (mbrFromRow r, [acctFrom (drop 4 r)]))

acctSaveDefault :: PG.Connection -> Integer -> Integer -> IO ()
acctSaveDefault dbCn cpId mbrId = 
  let insertAcct acctType = 
        DB.run dbCn "\
         \insert into MemberEquityAccount \
         \values (\
         \  ?,\
         \  ?,\
         \ (select coalesce(max(acctId),0)+1 from MemberEquityAccount),\
         \ ?)" 
           [DB.toSql cpId, DB.toSql mbrId, DB.toSql $ show acctType] 
  in do 
    insertAcct BuyInAcct
    insertAcct RollingPatronageAcct
    DB.commit dbCn