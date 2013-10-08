module Service.MemberEquityAccount
where

import Types
import Utils
import Persist.Persist
import Domain
import Serialize

import Happstack.Lite (path, dir, ServerPart(..), lookBS) 
import Happstack.Server (look)
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

getActionsForMemberEquityAcct :: PersistConnection -> PG.Connection -> ServerPartR
getActionsForMemberEquityAcct ref dbCn = do 
  cpId <- getSessionCoopId ref
  mbrId <- lookRead "mbrId"
  acctId <- lookRead "acctId"
  all <- liftIO $ acnGetFor dbCn cpId mbrId acctId
  let (acns,rstOfs) = unzip all
  let acnBals = runningBalance acns
  okJSResp $ reverse $ zipWith (\(a,b) c -> (a,c,b)) acnBals rstOfs
  
--probably not necessary, just one mem at a time, merge with below
getAllMembersEquityAccounts :: PersistConnection -> PG.Connection -> ServerPartR
getAllMembersEquityAccounts ref dbCn = do 
  cpId <- getSessionCoopId ref
  (liftIO $ acctGetAll dbCn cpId) >>= okJSResp

getMemberEquityAccount :: PersistConnection -> PG.Connection -> ServerPartR
getMemberEquityAccount ref dbCn = do
  cpId <- getSessionCoopId ref
  mbrId <- lookRead "mbrId"
  acctId <- lookRead "acctId"
  (liftIO $ acctGet dbCn cpId mbrId acctId) >>= okJSResp

putEquityAction :: PersistConnection -> PG.Connection -> ServerPartR
putEquityAction ref dbCn = do
  cpId <- getSessionCoopId ref  
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
 
