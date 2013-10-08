{-# Language DeriveDataTypeable #-}
module Type.WorkPatronage
where

import Type.Base

data WorkPatronage = WorkPatronage { -- ptrng
  work::Integer, 
  skillWeightedWork::Integer,
  seniority::Integer,
  quality::Integer,
  revenueGenerated::Integer,
  performedOver::FiscalPeriod
} deriving (Show, Eq, Ord, Data, Typeable)

instance Default WorkPatronage where
  def = WorkPatronage{work=0,skillWeightedWork=0,seniority=0,quality=0,
                      revenueGenerated=0,performedOver=def}
