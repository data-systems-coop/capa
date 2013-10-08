module Utils (
  module Utils,
  module System.Log.Logger,
  printf,
  module Data.Maybe
) where

import Happstack.Lite(ServerPart, ToMessage(..), lookText, Response(..), ok)
import Data.Aeson(ToJSON, encode)
import Numeric (readFloat)  -- num util
import qualified Data.ByteString.Char8 as B  -- + templates
import qualified Data.Text.Lazy as T  -- req, serialize
import qualified Data.Time as DT 
import qualified Happstack.Server as HS
import System.Log.Logger
import Text.Printf(printf)
import Data.Maybe

lookRead :: Read a => String -> ServerPart a
lookRead = fmap read . HS.look 

readRational :: String -> Rational  -- round to 100ths
readRational = toRational . fst . head . readFloat

newtype JSONData a = JSONData{ getJSONData :: a }

instance ToJSON a => ToMessage (JSONData a) where
  toMessage (JSONData d) = encode d
  toContentType _ = B.pack ("application/json")
  
type SessionID = String
type ServerPartR = ServerPart Response

okJSResp :: ToJSON a => a -> ServerPartR
okJSResp = ok . toResponse . JSONData

getCurrentDay :: IO DT.Day
getCurrentDay = fmap DT.utctDay DT.getCurrentTime

--start from current day
--  find closest quarter or year start prior to today
--  compute 2 years forward and back. 
--  enun starting from 2 years back
--  reverse
