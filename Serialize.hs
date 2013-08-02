{-# Language OverloadedStrings #-}
module Serialize 
where

import Types
  
import Data.Aeson                         
import qualified Data.Aeson.Types as AT
import qualified Data.Aeson.Generic as AG
import Control.Applicative
import Data.Attoparsec.Number as AN
import qualified Data.Vector as V
import Data.Time (Day, fromGregorian , toGregorian, UTCTime(..)) 
import qualified Data.Text as DT  

  

instance ToJSON Member where
  toJSON Member{firstName=fn, memberId=i} = 
  	 object ["firstName" .= fn,
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

instance ToJSON MemberEquityAction where
  toJSON MemberEquityAction{actionType=act,amount=amt,performedOn=prf} = 
  	 object ["actionType" .= AG.toJSON act, 
	 	 "amount" .= amt, 
	 	 "performedOn" .= toGregorian prf]

instance ToJSON GregorianMonth where
  toJSON (GregorianMonth year month) = 
    	 object ["year" .= year, 
	 	 "month" .= month]

instance ToJSON FiscalPeriod where
  toJSON FiscalPeriod{start=st,periodType=pt} = 
  	 object ["start" .= toJSON st,
	  	 "periodType" .= AG.toJSON pt]

instance ToJSON FinancialResults where
  toJSON FinancialResults{over=ov,surplus=sr,allocatedOn=ao} = 
         object ["over" .= toJSON ov,
                 "surplus" .= sr, 
                 "allocatedOn" .= toJSON ao]

instance ToJSON Day where
  toJSON d = 
    let (yr,mo,dy) = toGregorian d
    in Array $ V.fromList [toJSON yr,toJSON mo,toJSON dy]

--FromQParams for PatronageWeights, WorkPatronage, FinancialResults, MemberEqAct

instance FromJSON MemberEquityAction where
  parseJSON (Object v) = 
    MemberEquityAction <$> 
      v .: "actionType" <*> 
      v .: "amount" <*> 
      v .: "performedOn"
  
instance FromJSON EquityActionType where
  parseJSON (String t) = pure $ read $ DT.unpack t

instance FromJSON GregorianMonth where
  parseJSON (Object v) = 
     GregorianMonth <$> v .: "year" <*> v.: "month"

instance FromJSON PeriodType where
  parseJSON (String t) = pure $ read $ DT.unpack t
     
instance FromJSON FiscalPeriod where
  parseJSON (Object v) = 
     FiscalPeriod <$> v .: "start" <*> v .: "periodType"

instance FromJSON Day where
  parseJSON (Array a) = 
     fromGregorian 
       <$> withNumber "inccorect num" (\(AN.I i) -> pure i) (a V.! 0)
       <*> withNumber "inccorect num" (\(AN.I i) -> pure $ fromIntegral i) (a V.! 1)
       <*> withNumber "inccorect num" (\(AN.I i) -> pure $ fromIntegral i) (a V.! 2)

instance FromJSON SeniorityMappingEntry where
  parseJSON (Object v) = 
    SeniorityMappingEntry <$> v .: "start"
