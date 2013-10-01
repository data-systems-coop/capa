{-# Language DeriveDataTypeable #-}
module Type.ActionEvent
where

import Data.Data(Data, Typeable)
import Data.Time(Day)
import Data.Ratio ((%))
import Data.Default

import Type.Base

data Disbursal = Disbursal {
  dsbPerformedOn::Day, dsbProportion::Rational
} deriving (Show, Read, Eq, Ord, Data, Typeable)

data Allocation = Allocation {
  alcPerformedOn::Day
} deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Default Disbursal where
  def = Disbursal{dsbPerformedOn=def,dsbProportion=1}

instance Default Allocation where
  def = Allocation{alcPerformedOn=def}
