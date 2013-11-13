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

import qualified Data.ByteString.Lazy.Char8 as B

getAllFinancialResultsDetail 
  :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllFinancialResultsDetail = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (liftIO $ rsltGetAll dbCn cpId) >>= (lift . okJSResp)
       
postFinancialResults
  :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
postFinancialResults = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd  
  lift $ do 
    res <- parseObject 
    (liftIO $ rsltSaveFor dbCn cpId res) >>= okJSResp

deleteFinancialResults
  :: String -> ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
deleteFinancialResults overStr = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd  
  lift $ do 
    let Just over = decode $ B.pack overStr --skip packing
    (liftIO $ rsltDelete dbCn cpId over) >>= okJSResp
