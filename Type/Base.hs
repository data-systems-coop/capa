{-# Language DeriveDataTypeable #-}
module Type.Base (
  module Type.Base, 
  Data, Typeable,
  Day, fromGregorian, toGregorian,
  module Data.Default
) where

import Data.Data(Data, Typeable)  -- allow persist, serialize
import Data.Time(Day)
import Data.Time(fromGregorian, toGregorian)
import Data.Default

import qualified Data.Map as M

data FiscalPeriod = FiscalPeriod {  -- prd
  start::GregorianMonth,
  periodType::PeriodType
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data GregorianMonth = GregorianMonth Year Month
  deriving (Show, Read, Eq, Ord, Data, Typeable)
           
toDay (GregorianMonth yr mo) = fromGregorian yr mo 1
  
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
