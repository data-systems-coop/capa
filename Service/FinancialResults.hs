{-# Language ScopedTypeVariables #-}
module Service.FinancialResults
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Service.Security

getAllFinancialResultsDetail 
  :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllFinancialResultsDetail = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (liftIO $ rsltGetAll dbCn cpId) >>= (lift . okJSResp)
       
putFinancialResults
  :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
putFinancialResults = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd  
  lift $ do 
    surplus <- lookRead "surplus"
    overStr <- lookBS "over"
    let Just over = decode overStr
    let res = FinancialResults over surplus Nothing
    (liftIO $ rsltSaveFor dbCn cpId res) >>= okJSResp

deleteFinancialResults
  :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
deleteFinancialResults = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd  
  lift $ do 
    overStr <- lookBS "over"
    let Just over = decode overStr
    (liftIO $ rsltDelete dbCn cpId over) >>= okJSResp
