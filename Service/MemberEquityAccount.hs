module Service.MemberEquityAccount
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Service.Security

getActionsForMemberEquityAcct ::
  Integer -> Integer -> 
    ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getActionsForMemberEquityAcct mbrId acctId = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    all <- liftIO $ acnGetFor dbCn cpId mbrId acctId
    let (acns,rstOfs) = unzip all
    let acnBals = runningBalance acns
    okJSResp $ reverse $ zipWith (\(a,b) c -> (a,c,b)) acnBals rstOfs
  
--probably not necessary, just one mem at a time, merge with below
getAllMembersEquityAccounts ::
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllMembersEquityAccounts = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (liftIO $ acctGetAll dbCn cpId) >>= (lift . okJSResp)

getMemberEquityAccount :: 
  Integer -> Integer -> 
    ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getMemberEquityAccount mbrId acctId = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (liftIO $ acctGet dbCn cpId mbrId acctId) >>= (lift . okJSResp)

postEquityAction :: 
  Integer -> Integer -> 
    ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
postEquityAction mbrId acctId = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    overVal <- fmap decode $ lookBS "resultOf"
    acn <- parseObject
    (liftIO $ acnSaveFor dbCn cpId mbrId acctId overVal acn) >>= okJSResp
 
