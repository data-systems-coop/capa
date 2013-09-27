{-# Language DeriveDataTypeable #-}
module Type.ActionEvent
where

import Data.Data(Data, Typeable)
import Data.Time(Day)
import Data.Ratio ((%))
import Data.Default

import Type.Base

data ActionEvent = 
  Disbursal {
    dsbPerformedOn::Day, dsbProportion::Rational
  } | 
  Allocation {
    alcPerformedOn::Day
  } 
 deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Default ActionEvent where
  def = Disbursal{dsbPerformedOn=def,dsbProportion=def}
  
   
  