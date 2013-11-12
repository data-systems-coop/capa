{-# Language ScopedTypeVariables #-}
module Service.Member
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Service.Security

postMemberAndAccounts :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
postMemberAndAccounts = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  mem <- lift $ parseObject 
  (liftIO $ do 
    mbrId <- mbrSave dbCn cpId mem
    acctSaveDefault dbCn cpId mbrId) >>= (lift . okJSResp)
     
getMember :: 
  Integer -> ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getMember mid = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do
    (liftIO $ mbrGet dbCn cpId mid) >>= okJSResp

getMembers :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getMembers = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (liftIO $ getCurrentDay >>= mbrGetAll dbCn cpId) >>= (lift . okJSResp)
