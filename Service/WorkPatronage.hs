{-# Language ScopedTypeVariables #-}
module Service.WorkPatronage
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Data.Map as M

import qualified Data.ByteString.Lazy.Char8 as LB 

import Service.Security

lookPatronage :: AllocationMethod -> FiscalPeriod -> ServerPartT IO WorkPatronage
lookPatronage method performedOver = 
  case method of 
    ProductiveHours -> do
      work <- lookRead "work"
      return def{work=work,performedOver=performedOver}
    Wages -> do
      skillWeightedWork <- lookRead "skillWeightedWork"
      return def{skillWeightedWork=skillWeightedWork,performedOver=performedOver}
    _ -> do
      work <- lookRead "work"
      skillWeightedWork <- lookRead "skillWeightedWork"
      return def{work=work,skillWeightedWork=skillWeightedWork,
                 performedOver=performedOver}

postMemberPatronage 
  :: Integer -> FiscalPeriod -> 
       ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
postMemberPatronage mbrId performedOver = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    (allocMethod, _) <- liftIO $ allocStngGet dbCn cpId
    lookPatronage allocMethod performedOver >>= 
      (liftIO . ptrngSaveFor dbCn cpId mbrId) >>= okJSResp

deleteMemberPatronage 
  :: Integer -> FiscalPeriod -> 
       ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
deleteMemberPatronage mbrId performedOver = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    (liftIO $ ptrngDelete dbCn cpId mbrId performedOver) >>= okJSResp

getAllMemberPatronage 
  :: FiscalPeriod -> ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllMemberPatronage performedOver = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do
    mpAll <- 
      liftIO $ do
        mpngs <- snrtyMpngsGet dbCn cpId
        ptrngGetFor dbCn cpId performedOver mpngs
    let (mp, mu) = M.partition isJust mpAll
    okJSResp $ (mp, M.keys mu)

decodePeriod :: String -> FiscalPeriod  --turn this into fromrequri instance
decodePeriod = fromJust . decode . LB.pack 
