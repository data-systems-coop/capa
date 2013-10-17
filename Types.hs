{-# Language DeriveDataTypeable #-}
module Types(     
  module Types,
  module Type.Base,
  module Type.WorkPatronage,
  module Type.Allocation,
  module Type.Disbursal
) where 

import Data.Ratio ((%))
import qualified Data.Map as M
import qualified Data.Set as S

import Type.Base
import Type.WorkPatronage
import Type.Allocation
import Type.Disbursal

data PatronageWeights = PatronageWeights { -- wght
  workw::Rational,
  skillWeightedWorkw::Rational,
  seniorityw::Rational,
  qualityw::Rational,
  revenueGeneratedw::Rational
} deriving (Show, Eq, Ord, Data, Typeable)

data MemberEquityAction = MemberEquityAction { -- acn
  actionType::EquityActionType,
  amount::Money,
  performedOn::Day
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data EquityActionType =   --migrate all actions in db, + name changes
  BuyIn |
  AllocatePatronageRebate | 
  DistributeInstallment |
  EarnInterest |
  DistributeOnDeparture |
  DistributeOnDissolution |
  DistributeMilestone |
  AllocateDelayedNonQualified
   deriving (Show, Read, Eq, Ord, Data, Typeable)  
            
acnEffectiveAmount MemberEquityAction{ actionType = actionType, amount = amount } 
  | S.member 
      actionType 
      (S.fromList 
         [BuyIn, AllocatePatronageRebate, EarnInterest, AllocateDelayedNonQualified]) = 
        amount
  | otherwise = -amount

data MemberEquityAccount = MemberEquityAccount {  -- acct
  ida::Integer,
  accountType::EquityAccountType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data EquityAccountType = BuyInAcct | RollingPatronageAcct
   deriving (Show, Read, Eq, Ord, Data, Typeable)

data Member = Member { -- mbr
  firstName::String, 
  lastName::String,
  memberId::Integer,
  acceptedOn::Day
} deriving (Show, Eq, Ord, Data, Typeable)

data FinancialResults = FinancialResults { -- rslt
  over::FiscalPeriod,
  surplus::Money,   --net-inc
  allocatedOn::Maybe Day  -- REMOVE
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data Cooperative = Cooperative { -- cp
  cooperativeId::Integer,
  name::String,
  username::OpenID,
  usageStart::Day,
  usageEnd::Maybe Day,
  fiscalCalendarType::FiscalCalendarType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data SeniorityMappingEntry = SeniorityMappingEntry { 
  snrtyMpEntStart::Years 
} deriving (Show, Read, Eq, Ord, Data, Typeable)
type SeniorityLevel = Integer
type SeniorityMappings = M.Map SeniorityMappingEntry SeniorityLevel




data FiscalCalendarType = FiscalCalendarType{ -- clTp
  startf::Month,
  periodTypef::PeriodType
} deriving (Show, Read, Eq, Ord, Data, Typeable)


type OpenID = String
type Money = Integer

data GregorianDuration = GregorianDuration Years Months 
  deriving (Show, Read, Eq, Ord, Data, Typeable)
type Years = Integer
type Months = Integer

type DisbursalSchedule = [(GregorianDuration, Rational)]

data AllocationMethod = 
  ProductiveHours | Wages | SimpleMix | SeniorityMix | ElaborateMix
  deriving (Show, Read, Eq, Ord, Data, Typeable)

data PatronageFieldDetail = PatronageFieldDetail {
  ptrngFldLabel::String
} deriving (Show, Read, Eq, Ord, Data, Typeable)

workFieldDetail = PatronageFieldDetail "work" 
skillWeightedWorkFieldDetail =  PatronageFieldDetail "skillWeightedWork"
seniorityFieldDetail = PatronageFieldDetail "seniority" 
qualityFieldDetail = PatronageFieldDetail "quality" 
revenueGeneratedFieldDetail = PatronageFieldDetail "revenueGenerated" 


  
instance Default GregorianDuration where
  def = GregorianDuration 0 0

instance Default AllocationMethod where
  def = ProductiveHours

  
instance Default EquityActionType where
  def = AllocatePatronageRebate

instance Default PatronageWeights where 
  def = PatronageWeights{workw=1,skillWeightedWorkw=def,seniorityw=def,qualityw=def,
                         revenueGeneratedw=def}


instance Default MemberEquityAction where
  def = MemberEquityAction{actionType=def, amount=def,performedOn=def}

instance Default EquityAccountType where
  def = RollingPatronageAcct

instance Default MemberEquityAccount where
  def = MemberEquityAccount{ida=1,accountType=def}

instance Default Member where
  def = Member{firstName=def,lastName=def,memberId=1,acceptedOn=def}
  
instance Default FinancialResults where
  def = FinancialResults{over=def,surplus=def,allocatedOn=def}
  
instance Default FiscalCalendarType where
  def = FiscalCalendarType{startf=1,periodTypef=def}

instance Default Cooperative where
  def = Cooperative{cooperativeId=1,name=def,username=def,usageStart=def,usageEnd=def,
                    fiscalCalendarType=def}


-- sample data
(f1,f2) = 
  (def{start=(GregorianMonth 2012 1)},
   def{start=(GregorianMonth 2011 1)})::(FiscalPeriod,FiscalPeriod)
(m1, m2, m3) = 
   (Member "John" "Smith" 1 (fromGregorian 2010 1 1), 
    Member "Kanishka" "Azimi" 2 (fromGregorian 2011 1 1), 
    Member "Dave" "Jackson" 3 (fromGregorian 2009 3 30))::(Member,Member,Member)
pw1 = def{workw=7%10,skillWeightedWorkw=3%10}::PatronageWeights
coop1 = def{name="Coop1",username="https://www.google.com/profiles/102678013619698873278",
             usageStart=fromGregorian 2010 1 1}
settings1 = (SimpleMix, 
             pw1,
             [(GregorianDuration 1 0, 6%10), (GregorianDuration 1 6, 4%10)])
                 -- add seniority levels
memPatronage1 = (M.fromList 
                  [ (m1, [WorkPatronage 5 4 3 2 1 f2])
                  , (m2, [WorkPatronage 10 20 30 40 50 f2])
                  , (m3, [WorkPatronage 100 200 300 400 500 f2]) ])
res1 = [FinancialResults 
          def{start=GregorianMonth 2012 1} 200 $ Just (fromGregorian 2011 1 2)] 

