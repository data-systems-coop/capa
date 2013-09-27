{-# Language DeriveDataTypeable #-}
module Type.Base
where

import Data.Data(Data, Typeable)  -- allow persist, serialize
import Data.Time(Day)
import Data.Time(fromGregorian)
import qualified Data.Map as M
import Data.Default

data FiscalPeriod = FiscalPeriod {  -- prd
  start::GregorianMonth,
  periodType::PeriodType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data GregorianMonth = GregorianMonth Year Month
  deriving (Show, Read, Eq, Ord, Data, Typeable)
  
data PeriodType = Year | Quarter
  deriving (Show, Read, Eq, Ord, Data, Typeable)

type Year = Integer
type Month = Int

instance Default PeriodType where
  def = Year

instance Default GregorianMonth where
  def = GregorianMonth 1970 1

instance Default FiscalPeriod where
  def = FiscalPeriod{start=def,periodType=def}

instance Default Day where
  def = fromGregorian 1970 1 1
