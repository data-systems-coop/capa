module Persist.Base (
  module Types,
  module Utils,
  module Persist.Base,
  printf,
  fromGregorian, toGregorian, Day(..),
  void,
  module Data.Maybe,
  module System.Log.Logger,
  Connection,
  module Database.HDBC
) where


import Utils
import Types

import Data.Maybe
import System.Log.Logger
import Text.Printf(printf)
import Data.Time (fromGregorian, toGregorian, Day(..))

import Data.Data            ( Data, Typeable ) 
import qualified Data.Map as M
import Control.Monad.Reader ( ask )      
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update, makeAcidic )
import Data.SafeCopy        ( base, deriveSafeCopy )

import Database.HDBC.PostgreSQL(Connection)
import Database.HDBC 

import Control.Monad ( void )

--generally prefer sets not lists

prdToSql :: FiscalPeriod -> (SqlValue, SqlValue)
prdToSql FiscalPeriod{start=GregorianMonth yr mo, periodType = prdType} = 
  (toSql $ fromGregorian yr mo 1, toSql $ show prdType)

prdFrom :: [SqlValue] -> FiscalPeriod --use more
prdFrom (start:prdType:_) = 
  let (yr,mo,_) = toGregorian $ fromSql start
  in FiscalPeriod (GregorianMonth yr mo) (read $ fromSql prdType)

toSqlDouble :: Rational -> SqlValue
toSqlDouble = toSql . (fromRational::Rational -> Double)

