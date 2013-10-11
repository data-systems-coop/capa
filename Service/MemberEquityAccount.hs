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
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getActionsForMemberEquityAcct = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    mbrId <- lookRead "mbrId"
    acctId <- lookRead "acctId"
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
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getMemberEquityAccount = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  mbrId <- lift $ lookRead "mbrId"
  acctId <- lift $ lookRead "acctId"
  (liftIO $ acctGet dbCn cpId mbrId acctId) >>= (lift . okJSResp)

putEquityAction :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
putEquityAction = do
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    mbrId <- lookRead "mbrId"
    acctId <- lookRead "acctId"
    actionType <- lookRead "actionType"
    amount <- lookRead "amount"
    performedOnStr <- look "performedOn"
    let Just performedOn = parseJSDate performedOnStr
    overStr <- lookBS "resultOf"
    let overVal = decode overStr
    let acn = MemberEquityAction{actionType=actionType,amount=amount,
                                 performedOn=performedOn}
    (liftIO $ acnSaveFor dbCn cpId mbrId acctId overVal acn) >>= okJSResp
 
