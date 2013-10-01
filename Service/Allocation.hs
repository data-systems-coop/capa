module Service.Allocation
where

import Types
import Utils
import Persist
import Domain
import Serialize

import Happstack.Lite (path, dir, ServerPart(..), lookBS) 
import qualified Data.Maybe as MB
import qualified Data.List as L            
import Data.Time (fromGregorian , toGregorian, UTCTime(..), getCurrentTime,
                  addGregorianMonthsClip, addGregorianYearsClip)   
import Data.Map as M
import Data.Default
import Data.Aeson (encode, decode)
import Control.Monad.IO.Class (liftIO)  

import qualified Database.HDBC.PostgreSQL as PG -- remove me

import Control.Monad(guard, void)

import System.Log.Logger as LG
import Text.Printf(printf)

import Service.Security

postAllocateToMembers :: PersistConnection -> PG.Connection -> ServerPartR
postAllocateToMembers ref dbCn = 
  handleAllocateToMembers ref dbCn >>= okJSResp . snd

postScheduleAllocateDisbursal :: PersistConnection -> PG.Connection -> ServerPartR
postScheduleAllocateDisbursal ref dbCn = 
  do cpId <- getSessionCoopId ref
     allocateActionStr <- lookBS "allocateAction"
     let Just allocateAction = decode allocateActionStr
     disbursalSchedule <- liftIO $ dsbSchedGet dbCn cpId
     okJSResp $ scheduleDisbursalsFor allocateAction $ disbursalSchedule

handleAllocateToMembers ::
  PersistConnection -> PG.Connection -> 
   ServerPart (FiscalPeriod, (M.Map Member MemberEquityAction))
handleAllocateToMembers ref dbCn =  
  do cpId <- getSessionCoopId ref
     overStr <- lookBS "over"
     let Just allocateOver = decode overStr
     liftIO $ do 
       today <- getCurrentDay
       Just res <- rsltGetForOver dbCn cpId allocateOver
       patronage <- ptrngGetFor dbCn cpId allocateOver     
       (name, parameters) <- allocStngGet dbCn cpId
       return $
         (allocateOver, 
          allocateEquityFor res today (parameters, (M.map MB.fromJust patronage)))

postAllocationDisbursal :: PersistConnection -> PG.Connection -> ServerPartR
postAllocationDisbursal ref dbCn = 
  do cpId <- getSessionCoopId ref
     (allocateOver, me) <- handleAllocateToMembers ref dbCn
     (liftIO $ do 
       LG.infoM "Service.postAllocationDisbursal" $ 
         printf "%d. alloc for %s" cpId (show allocateOver)            
       disbursalSchedule <- dsbSchedGet dbCn cpId     
       mapM_
         (\(Member{memberId=mbrId}, allocAcn) -> do
           acnSaveToRolling dbCn cpId mbrId allocateOver allocAcn
           mapM_ 
             (acnSaveToRolling dbCn cpId mbrId allocateOver)
             (scheduleDisbursalsFor allocAcn disbursalSchedule))
         (M.toList me)
       today <- getCurrentDay
       rsltUpdateAllocated dbCn cpId allocateOver today) >>= okJSResp
        
getAllocation::PersistConnection -> PG.Connection -> ServerPartR 
getAllocation ref dbCn = do 
  cpId <- getSessionCoopId ref
  Just resultOf <- readPeriod "resultOf"
  (liftIO $ allocGet dbCn cpId resultOf) >>= okJSResp
    
getAllocationDisbursals::PersistConnection -> PG.Connection -> ServerPartR
getAllocationDisbursals ref dbCn = do 
  cpId <- getSessionCoopId ref
  Just resultOf <- readPeriod "resultOf"
  (liftIO $ 
    disbursalGetFor dbCn cpId resultOf >>= 
    mapM 
      (\d -> 
        fmap ((,) d) $ acnGetForDisbursal dbCn cpId resultOf $ dsbPerformedOn d)) >>= 
    okJSResp
{--
postRescheduleDisbursal::PersistConnection -> PG.Connection -> ServerPartR
postRescheduleDisbursal ref dbCn = do cpId <- getSessionCoopId ref
  -- read (identifying allocdate, dsbdate) + new date
--}
--postForkDisbursal::PersistConnection -> PG.Connection -> ServerPartR
  -- read (identifying allocdate, dsbdate) + new date, updated proportion, new proport

--postShiftProportionDisbursals::PersistConnection -> PG.Connection -> ServerPartR
  -- read (identifying allocdate, dsbdate from, dsbdate to, proportion)

readPeriod::String -> ServerPart (Maybe FiscalPeriod)
readPeriod = fmap decode . lookBS