{-# Language DeriveDataTypeable #-}
module Type.Disbursal
where

import Data.Ratio ((%))
import Type.Base

data Disbursal = Disbursal {
  dsbPerformedOn::Day, dsbProportion::Rational
} deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Default Disbursal where
  def = Disbursal{dsbPerformedOn=def,dsbProportion=1}
