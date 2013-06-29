{-# Language DeriveDataTypeable #-}
module Types
    
where 

import Data.Data(Data, Typeable)  -- allow persist, serialize
import Data.Time(Day)
import Data.Ratio ((%))
import Data.Time(fromGregorian)
import qualified Data.Map as M
-- import Data.Default 

data WorkPatronage = WorkPatronage {
  work::Integer, 
  skillWeightedWork::Integer,
  seniority::Integer,
  quality::Integer,
  revenueGenerated::Integer,
  performedOver::FiscalPeriod
} deriving (Show, Eq, Ord, Data, Typeable)

data PatronageWeights = PatronageWeights {
  workw::Rational,
  skillWeightedWorkw::Rational,
  seniorityw::Rational,
  qualityw::Rational,
  revenueGeneratedw::Rational
} deriving (Show, Eq, Ord, Data, Typeable)

data MemberEquityAction = MemberEquityAction {
  actionType::EquityActionType,
  amount::Money,
  performedOn::Day
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data EquityActionType = 
  BuyIn |
  AllocatePatronageRebate | 
  DistributeImmediate | 
  DistributeInstallment |
  EarnInterest |
  DistributeOnDeparture |
  DistributeOnDissolution |
  DistributeMilestone |
  AllocateDelayedNonQualified
   deriving (Show, Read, Eq, Ord, Data, Typeable)  
     
data MemberEquityAccount = MemberEquityAccount {  
  ida::Integer,
  accountType::EquityAccountType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data EquityAccountType = Committed | RollingPatronage
   deriving (Show, Read, Eq, Ord, Data, Typeable)

data Member = Member {
  firstName::String, 
  --lastName::String
  memberId::Integer
  --acceptedOn::Day
  --leftOn::Day
} deriving (Show, Eq, Ord, Data, Typeable)

data FinancialResults = FinancialResults { 
  over::FiscalPeriod,
  surplus::Money,   --net-inc
  allocatedOn::Maybe Day
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data Cooperative = Cooperative {
  cooperativeId::Integer,
  name::String,
  username::Email,
  bookkeeperFirstName::String,
  usageStart::Day,
  usageEnd::Day,
  fiscalCalendarType::FiscalCalendarType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

type SeniorityMapping = [(Years,Years),SeniorityLevel]
type SeniorityLevel = Integer

data FiscalPeriod = FiscalPeriod {
  start::GregorianMonth,
  periodType::PeriodType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data GregorianMonth = GregorianMonth Year Month
  deriving (Show, Read, Eq, Ord, Data, Typeable)

type Year = Integer
type Month = Int

data FiscalCalendarType = FiscalCalendarType{
  startf::Month,
  periodTypef::PeriodType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data PeriodType = Year | Quarter
  deriving (Show, Read, Eq, Ord, Data, Typeable)

type Email = String
type Money = Integer

data GregorianDuration = GregorianDuration Years Months 
  deriving (Show, Read, Eq, Ord, Data, Typeable)
type Years = Integer
type Months = Integer

type DisbursalSchedule = [(GregorianDuration, Rational)]
-- [(GregorianDuration 0 6, 1%4), (GregorianDuration 1 0, 3%4)]

type AllocationMethod = String

-- sample data
f1 = FiscalPeriod (GregorianMonth 2012 1) Year
f2 = FiscalPeriod (GregorianMonth 2011 1) Year
(m1, m2, m3) = (Member "Jonh" 1, Member "Kanishka" 2, Member "Dave" 3)
pw1 = PatronageWeights (5%10) (3%10) (2%10) 0 0
coop1 = (Cooperative 
           1 "Coop1" "k@m.com" "John" 
           (fromGregorian 2010 1 1) 
           (fromGregorian 2010 2 2) (FiscalCalendarType 1 Year))
settings1 = 
              (Just 
                ("Basic", 
                 pw1,
                 [(GregorianDuration 1 0, 6%10), (GregorianDuration 1 6, 4%10)]))
memPatronage1 = 
              (M.fromList [ (m1, [WorkPatronage 5 4 3 2 1 f2])
                          , (m2, [WorkPatronage 10 20 30 40 50 f2])
                          , (m3, [WorkPatronage 100 200 300 400 500 f2]) ])
res1 = 
              [FinancialResults 
                  (FiscalPeriod (GregorianMonth 2012 1) Year) 
                  200
                  $ Just (fromGregorian 2011 1 2)] 
allocs1 = 
              (M.fromList [(FinancialResults  
                              (FiscalPeriod (GregorianMonth 2012 1) Year)
                              200
                              $ Just (fromGregorian 2011 1 2), 
                            [])])

