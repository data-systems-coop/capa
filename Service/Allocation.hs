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

postAllocateToMembers :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
postAllocateToMembers = 
  handleAllocateToMembers >>= lift . okJSResp . snd

postScheduleAllocateDisbursal :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
postScheduleAllocateDisbursal = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
    allocateActionStr <- lookBS "allocateAction"
    let Just allocateAction = decode allocateActionStr
    disbursalSchedule <- liftIO $ dsbSchedGet dbCn cpId
    okJSResp $ scheduleDisbursalsFor allocateAction $ disbursalSchedule

handleAllocateToMembers ::
  ReaderT (PersistConnection, Connection) 
          (ServerPartT IO) (FiscalPeriod, (M.Map Member (MemberEquityAction, Rational)))
handleAllocateToMembers = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  lift $ do 
     overStr <- lookBS "over"
     let Just allocateOver = decode overStr
     liftIO $ do 
       today <- getCurrentDay
       Just res <- rsltGetForOver dbCn cpId allocateOver
       patronage <- ptrngGetFor dbCn cpId allocateOver     
       (name, parameters) <- allocStngGet dbCn cpId
       return $
         (allocateOver, 
          allocateEquityFor res today (parameters, (M.map fromJust patronage)))

postAllocationDisbursal :: 
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
postAllocationDisbursal = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  (allocateOver, me) <- handleAllocateToMembers
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
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllocation = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  Just resultOf <- lift $ readPeriod "resultOf"
  (liftIO $ allocGet dbCn cpId resultOf) >>= (lift . okJSResp)
    
--get allocation actions

getAllocationDisbursals ::
  ReaderT (PersistConnection, Connection) (ServerPartT IO) Response
getAllocationDisbursals = do 
  cpId <- withReaderT fst getSessionCoopId
  dbCn <- asks snd
  Just resultOf <- lift $ readPeriod "resultOf"
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

readPeriod::String -> ServerPartT IO (Maybe FiscalPeriod)
readPeriod = fmap decode . lookBS