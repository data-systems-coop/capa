{-# Language ScopedTypeVariables #-}
module Service.Member
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Happstack.Lite (path) 

import Service.Security

putMemberAndAccounts :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
putMemberAndAccounts = do 
  cpId <- withReaderT fst getSessionCoopId
  mem <- lift $ parseObject 
  dbCn <- asks snd
  (liftIO $ do 
    mbrId <- mbrSave dbCn cpId mem
    acctSaveDefault dbCn cpId mbrId) >>= (lift . okJSResp)
     
getMember :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getMember = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ path $ \(mid::Integer) -> do
    (liftIO $ mbrGet dbCn cpId mid) >>= okJSResp

getMembers :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getMembers = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (liftIO $ getCurrentDay >>= mbrGetAll dbCn cpId) >>= (lift . okJSResp)
