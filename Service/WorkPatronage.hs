{-# Language ScopedTypeVariables #-}
module Service.WorkPatronage
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Happstack.Lite (path, dir) 
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
      case method of 
        SimpleMix -> 
          return def{work=work,skillWeightedWork=skillWeightedWork,
                     performedOver=performedOver}
        _ -> do
          seniority <- lookRead "seniority"
          return def{work=work,skillWeightedWork=skillWeightedWork,
                               seniority=seniority,performedOver=performedOver}

putMemberPatronage 
  :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
putMemberPatronage = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ path $ \(idIn::Integer) -> 
    dir "patronage" $ path $ \(performedOverStr::String) -> do
	let Just performedOver = decode $ LB.pack performedOverStr        
        (allocMethod, _) <- liftIO $ allocStngGet dbCn cpId
        lookPatronage allocMethod performedOver >>= 
          (liftIO . ptrngSaveFor dbCn cpId idIn) >>= okJSResp

getAllMemberPatronage 
  :: ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllMemberPatronage = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ path $ \(fiscalPeriodStr::String) -> do
    let Just fiscalPeriod = decode $ LB.pack fiscalPeriodStr
    mpAll <- liftIO $ ptrngGetFor dbCn cpId fiscalPeriod 
    let (mp, mu) = M.partition isJust mpAll
    okJSResp $ (mp, M.keys mu)


