module Service.Allocation
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize
import Service.Base

import Data.Map as M

import Service.Security

import Control.Monad.Reader

postAllocateToMembers :: 
  FiscalPeriod -> ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
postAllocateToMembers allocateOver = 
  handleAllocateToMembers allocateOver >>= lift . okJSResp

postScheduleAllocateDisbursal :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
postScheduleAllocateDisbursal = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    allocateAction <- lookDecode "allocateAction"
    disbursalSchedule <- liftIO $ dsbSchedGet dbCn cpId
    okJSResp $ scheduleDisbursalsFor allocateAction $ disbursalSchedule

handleAllocateToMembers ::
  FiscalPeriod -> 
    ReaderT (PersistConnection, Connection) 
            (ServerPartT IO) 
            (M.Map Member (MemberEquityAction, Rational))
handleAllocateToMembers allocateOver = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    liftIO $ do 
      today <- getCurrentDay
      Just res <- rsltGetForOver dbCn cpId allocateOver
      mpngs <- snrtyMpngsGet dbCn cpId
      patronage <- ptrngGetFor dbCn cpId allocateOver mpngs
      (name, parameters) <- allocStngGet dbCn cpId
      let acts = 
            runReader 
              (allocateEquityFor res today (parameters, (M.map fromJust patronage))) 
              mpngs
      return acts


postAllocationDisbursal :: 
  FiscalPeriod -> ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
postAllocationDisbursal allocateOver = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  me <- handleAllocateToMembers allocateOver
  lift $ do 
     (liftIO $ do 
       infoM "Service.postAllocationDisbursal" $ 
         printf "%d. alloc for %s" cpId (show allocateOver)
       disbursalSchedule <- dsbSchedGet dbCn cpId
       today <- getCurrentDay
       allocSave dbCn cpId allocateOver today
       mapM_
         (\(Member{memberId=mbrId}, (_, allocatedRatio)) ->
           allocAcnSaveToRolling dbCn cpId mbrId allocateOver allocatedRatio)
         (M.toList me)
       mapM_ 
         (disbursalSave dbCn cpId allocateOver)
         (scheduleDisbursals today disbursalSchedule)
       ) >>= okJSResp
        
getAllocation :: 
  FiscalPeriod -> ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllocation resultOf = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (liftIO $ allocGet dbCn cpId resultOf) >>= (lift . okJSResp)
    
--get allocation actions

getAllocationDisbursals :: 
  FiscalPeriod -> ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllocationDisbursals resultOf = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (liftIO $ 
    disbursalGetFor dbCn cpId resultOf >>= 
    mapM 
      (\d -> 
        fmap ((,) d) $ acnGetForDisbursal dbCn cpId resultOf $ dsbPerformedOn d)) >>= 
    (lift . okJSResp)
{--
postRescheduleDisbursal::PersistConnection -> Connection -> ServerPartR
postRescheduleDisbursal ref dbCn = do cpId <- getSessionCoopId ref
  -- read (identifying allocdate, dsbdate) + new date
--postForkDisbursal::PersistConnection -> Connection -> ServerPartR
  -- read (identifying allocdate, dsbdate) + new date, updated proportion, new proport
--postShiftProportionDisbursals::PersistConnection -> Connection -> ServerPartR
  -- read (identifying allocdate, dsbdate from, dsbdate to, proportion)
--}

