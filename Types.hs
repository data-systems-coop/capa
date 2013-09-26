{-# Language DeriveDataTypeable #-}
module Types
    
where 

import Data.Data(Data, Typeable)  -- allow persist, serialize
import Data.Time(Day)
import Data.Ratio ((%))
import Data.Time(fromGregorian)
import qualified Data.Map as M
import Data.Default

data WorkPatronage = WorkPatronage { -- ptrng
  work::Integer, 
  skillWeightedWork::Integer,
  seniority::Integer,
  quality::Integer,
  revenueGenerated::Integer,
  performedOver::FiscalPeriod
} deriving (Show, Eq, Ord, Data, Typeable)

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

data EquityActionType = 
  BuyIn |
  AllocatePatronageRebate | 
  DistributeInstallment |
  EarnInterest |
  DistributeOnDeparture |
  DistributeOnDissolution |
  DistributeMilestone |
  AllocateDelayedNonQualified
   deriving (Show, Read, Eq, Ord, Data, Typeable)  
     
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
  --leftOn::Maybe Day
} deriving (Show, Eq, Ord, Data, Typeable)

data FinancialResults = FinancialResults { -- rslt
  over::FiscalPeriod,
  surplus::Money,   --net-inc
  allocatedOn::Maybe Day
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

data FiscalPeriod = FiscalPeriod {  -- prd
  start::GregorianMonth,
  periodType::PeriodType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data GregorianMonth = GregorianMonth Year Month
  deriving (Show, Read, Eq, Ord, Data, Typeable)

type Year = Integer
type Month = Int

data FiscalCalendarType = FiscalCalendarType{ -- clTp
  startf::Month,
  periodTypef::PeriodType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data PeriodType = Year | Quarter
  deriving (Show, Read, Eq, Ord, Data, Typeable)

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

instance Default PatronageWeights where 
  def = PatronageWeights{workw=1,skillWeightedWorkw=def,seniorityw=def,qualityw=def,
                         revenueGeneratedw=def}
instance Default WorkPatronage where
  def = WorkPatronage{work=0,skillWeightedWork=0,
                               seniority=0,quality=0,revenueGenerated=0,
                               performedOver=FiscalPeriod (GregorianMonth 1970 1) Year}

-- sample data
(f1,f2) = 
  (FiscalPeriod (GregorianMonth 2012 1) Year,
   FiscalPeriod (GregorianMonth 2011 1) Year)
(m1, m2, m3) = 
   (Member "John" "Smith" 1 (fromGregorian 2010 1 1), 
    Member "Kanishka" "Azimi" 2 (fromGregorian 2011 1 1), 
    Member "Dave" "Jackson" 3 (fromGregorian 2009 3 30))
pw1 = PatronageWeights (7%10) (3%10) 0 0 0
coop1 = (Cooperative 
           1 "Coop1" "https://www.google.com/profiles/102678013619698873278"
           (fromGregorian 2010 1 1) Nothing (FiscalCalendarType 1 Year))
settings1 = (SimpleMix, 
             pw1,
             [(GregorianDuration 1 0, 6%10), (GregorianDuration 1 6, 4%10)])
                 -- add seniority levels
memPatronage1 = (M.fromList 
                  [ (m1, [WorkPatronage 5 4 3 2 1 f2])
                  , (m2, [WorkPatronage 10 20 30 40 50 f2])
                  , (m3, [WorkPatronage 100 200 300 400 500 f2]) ])
res1 = [FinancialResults 
          (FiscalPeriod (GregorianMonth 2012 1) Year) 
          200
          $ Just (fromGregorian 2011 1 2)] 
acct1 = MemberEquityAccount 1 RollingPatronageAcct
