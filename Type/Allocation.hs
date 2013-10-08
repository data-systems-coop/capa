{-# Language DeriveDataTypeable #-}
module Type.Allocation
where

import Type.Base
  
data Allocation = Allocation {
  alcPerformedOn::Day
} deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Default Allocation where
  def = Allocation{alcPerformedOn=def}
