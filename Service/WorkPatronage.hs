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

putMemberPatronage 
  :: Integer -> String -> 
       ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
putMemberPatronage idIn performedOverStr = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    let Just performedOver = decode $ LB.pack performedOverStr        
    (allocMethod, _) <- liftIO $ allocStngGet dbCn cpId
    lookPatronage allocMethod performedOver >>= 
      (liftIO . ptrngSaveFor dbCn cpId idIn) >>= okJSResp

deleteMemberPatronage 
  :: Integer -> String -> 
       ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
deleteMemberPatronage idIn performedOverStr = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    let Just performedOver = decode $ LB.pack performedOverStr        
    (liftIO $ ptrngDelete dbCn cpId idIn performedOver) >>= okJSResp

getAllMemberPatronage 
  :: String -> ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllMemberPatronage fiscalPeriodStr = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do
    let Just fiscalPeriod = decode $ LB.pack fiscalPeriodStr
    mpAll <- 
      liftIO $ do
        mpngs <- snrtyMpngsGet dbCn cpId
        ptrngGetFor dbCn cpId fiscalPeriod mpngs
    let (mp, mu) = M.partition isJust mpAll
    okJSResp $ (mp, M.keys mu)
    