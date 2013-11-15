{-# Language OverloadedStrings #-}
module Serialize 
where

import Types
import Utils  

import Data.Aeson                         
import qualified Data.Aeson.Types as AT
import qualified Data.Aeson.Generic as AG
import Control.Applicative
import Data.Attoparsec.Number as AN
import qualified Data.Vector as V
import Data.Time (Day, fromGregorian , toGregorian, UTCTime(..), getCurrentTime) 
import qualified Data.Text as DT  
import qualified Data.Map as M
import Happstack.Lite(ServerPart)
import Happstack.Server(look, lookBS, ServerPartT)
import Control.Monad.IO.Class (liftIO)  

instance ToJSON Cooperative where   
  toJSON Cooperative{cooperativeId=id,name=nm,username=usr,usageStart=strt,usageEnd=end,
                     fiscalCalendarType=cal} = 
    object ["cooperativeId" .= id, "name" .= nm, "username" .= usr, 
            "usageStart" .= strt,
            "usageEnd" .= end, "fiscalCalendarType" .= toJSON cal]

instance ToJSON Member where
  toJSON Member{firstName=fn, lastName=ln, acceptedOn=ac, memberId=i} = 
  	 object ["firstName" .= fn,
                 "lastName" .= ln,
                 "acceptedOn" .= toGregorian ac,
                 "memberId" .= i]

instance ToJSON WorkPatronage where
  toJSON WorkPatronage{work=work,skillWeightedWork=skillWeightedWork,
		       seniority=seniority,quality=quality,
		       revenueGenerated=revenueGenerated, performedOver=prf} = 
  	 object ["work" .= work, 
	 	 "skillWeightedWork" .= skillWeightedWork, 
		 "seniority" .= seniority, "quality" .= quality,
		 "revenueGenerated" .= revenueGenerated, 
		 "performedOver" .= toJSON prf]

instance ToJSON PatronageWeights where
  toJSON PatronageWeights{workw=workw,skillWeightedWorkw=skillWeightedWorkw,
                          seniorityw=seniorityw,qualityw=qualityw,
                          revenueGeneratedw=revenueGeneratedw} = 
         object ["workw" .= workw, "skillWeightedWorkw" .= skillWeightedWorkw,
                 "seniorityw" .= seniorityw, "qualityw" .= qualityw,
                 "revenueGeneratedw" .= revenueGeneratedw]

instance ToJSON MemberEquityAccount where
  toJSON MemberEquityAccount{ida=ida,accountType=typ} = 
    object ["ida" .= ida, "accountType" .= AG.toJSON typ]

instance ToJSON MemberEquityAction where
  toJSON MemberEquityAction{actionType=act,amount=amt,performedOn=prf} = 
  	 object ["actionType" .= AG.toJSON act, 
	 	 "amount" .= amt, 
	 	 "performedOn" .= toGregorian prf]

instance ToJSON Allocation where
  toJSON Allocation{alcPerformedOn=prf} = 
    object ["alcPerformedOn" .= toGregorian prf]

instance ToJSON Disbursal where
  toJSON Disbursal{dsbPerformedOn=prf,dsbProportion=prop} = 
    object ["dsbPerformedOn" .= toGregorian prf, "dsbProportion" .= prop]

instance ToJSON GregorianMonth where
  toJSON (GregorianMonth year month) = 
    	 object ["year" .= year, "month" .= month]

instance ToJSON FiscalPeriod where
  toJSON FiscalPeriod{start=st,periodType=pt} = 
  	 object ["start" .= toJSON st, "periodType" .= AG.toJSON pt]

instance ToJSON FinancialResults where
  toJSON FinancialResults{over=ov,surplus=sr,allocatedOn=ao} = 
         object ["over" .= toJSON ov,
                 "surplus" .= sr, 
                 "allocatedOn" .= toJSON ao]

instance ToJSON GregorianDuration where
  toJSON (GregorianDuration yrs mos) = 
         object["years" .= yrs, "months" .= mos]

instance ToJSON Day where
  toJSON d = 
    let (yr,mo,dy) = toGregorian d
    in Array $ V.fromList [toJSON yr,toJSON mo,toJSON dy]
       
instance ToJSON PatronageFieldDetail where
  toJSON PatronageFieldDetail{ptrngFldLabel=lbl} = object ["ptrngFldLabel" .= lbl] 

instance ToJSON SeniorityMappingEntry where
  toJSON SeniorityMappingEntry{snrtyMpEntStart=start} = object ["start" .= start]

instance ToJSON FiscalCalendarType where
  toJSON FiscalCalendarType{startf=st,periodTypef=prd} = 
    object["start" .= st, "periodType" .= AG.toJSON prd]

instance (ToJSON a, ToJSON b) => ToJSON (M.Map a b) where
  toJSON = toJSON . M.toList --should really be an object

--FromQParams for PatronageWeights, WorkPatronage, FinancialResults, MemberEqAct

instance FromJSON MemberEquityAction where
  parseJSON (Object v) = 
    MemberEquityAction <$> v .: "actionType" <*> v .: "amount" <*> v .: "performedOn"
  
instance FromJSON EquityActionType where
  parseJSON (String t) = pure $ read $ DT.unpack t

instance FromJSON GregorianMonth where
  parseJSON (Object v) = 
     GregorianMonth <$> v .: "year" <*> v.: "month"

instance FromJSON PeriodType where
  parseJSON (String t) = pure $ read $ DT.unpack t
     
instance FromJSON FiscalPeriod where
  parseJSON (Object v) = FiscalPeriod <$> v .: "start" <*> v .: "periodType"

instance FromJSON Day where
  parseJSON (Array a) = 
     fromGregorian 
       <$> withNumber "incorrect num" (\(AN.I i) -> pure i) (a V.! 0)
       <*> withNumber "incorrect num" (\(AN.I i) -> pure $ fromIntegral i) (a V.! 1)
       <*> withNumber "incorrect num" (\(AN.I i) -> pure $ fromIntegral i) (a V.! 2)

instance FromJSON SeniorityMappingEntry where
  parseJSON (Object v) = 
    SeniorityMappingEntry <$> v .: "start"

instance FromJSON GregorianDuration where 
  parseJSON (Object v) = GregorianDuration <$> v .: "years" <*> v .: "months"
  
parseJSDate :: String -> Maybe Day
parseJSDate str = 
  let (m,_:rs) = break (=='/') str
      (d,_:y) = break (=='/') rs
  in Just $ fromGregorian (read y) (read m) (read d)
     
class FromParams a where
  parseObject :: ServerPart a
  
instance FromParams Member where --new member
  parseObject = do 
    let mbrId = 0    
    firstName <- look "firstName"
    lastName <- look "lastName"
    Just acceptedOn <- fmap parseJSDate $ look "acceptedOn"
    return $ Member firstName lastName mbrId acceptedOn 

instance FromParams Cooperative where --new coop
  parseObject = do
    let cpId = 0
    name <- look "cpName"
    username <- look "username"
    UTCTime{utctDay=usageStart} <- liftIO getCurrentTime
    clTpStart <- lookRead "clTpStart"
    clTpPeriodType <- lookRead "clTpPeriodType"
    return $ 
      Cooperative cpId name username usageStart Nothing 
        (FiscalCalendarType clTpStart clTpPeriodType)

instance FromParams MemberEquityAction where --new action
  parseObject = do
    actionType <- lookRead "actionType"
    amount <- lookRead "amount"
    performedOnStr <- look "performedOn"
    let Just performedOn = parseJSDate performedOnStr
    return $ 
      MemberEquityAction{actionType=actionType,amount=amount,performedOn=performedOn}

instance FromParams FinancialResults where --new res
  parseObject = do
    surplus <- lookRead "surplus"
    over <- lookDecode "over"
    return $ FinancialResults over surplus Nothing


lookDecode :: (FromJSON a) => String -> ServerPartT IO a
lookDecode = fmap (fromJust . decode) . lookBS