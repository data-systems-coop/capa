{-# Language DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Persist.Member
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

mbrFromRow :: [DB.SqlValue] -> Member
mbrFromRow (mbrId:firstName:lastName:acceptedOn:_) = 
  Member (DB.fromSql firstName) (DB.fromSql lastName) (DB.fromSql mbrId)
    (DB.fromSql acceptedOn)
  
mbrGetAll :: PG.Connection -> Integer -> Day -> IO [(Member,Money)]
mbrGetAll dbCn cpId asOf =
  (DB.quickQuery' dbCn "select mbrId,firstName,lastName,acceptedOn, coalesce((select sum(amount) from MemberEquityAction where (cpId,mbrId) = (a.cpId,a.mbrId) and performedOn <= ?),0) as total from member a where cpId=?"
     [DB.toSql asOf, DB.toSql cpId]) >>=
    return . fmap (\r -> (mbrFromRow r,DB.fromSql $ r !! 4))

mbrGet :: PG.Connection -> Integer -> Integer -> IO (Maybe Member)
mbrGet dbCn cpId mbrId = 
  DB.quickQuery' dbCn "select mbrId,firstName,lastName,acceptedOn from member where (cpId,mbrId)=(?,?)"
    [DB.SqlInteger cpId, DB.SqlInteger mbrId] >>= 
  return . fmap mbrFromRow . MB.listToMaybe

mbrSave :: PG.Connection -> Integer -> Member -> IO Integer
mbrSave dbCn cpId Member{firstName=first,lastName=last,acceptedOn=acc}=do
  DB.run dbCn "insert into Member values (?,(select coalesce(max(mbrId),0)+1 from Member where cpId=?),?,?,?)" 
    [DB.toSql cpId, DB.toSql cpId, DB.toSql first, DB.toSql last, DB.toSql acc]
  [[mbrId]] <- 
    DB.quickQuery' dbCn "select max(mbrId) from Member where cpId=?" [DB.toSql cpId]
  DB.commit dbCn
  return $ DB.fromSql mbrId
