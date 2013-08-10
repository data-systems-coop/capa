module Utils 
where

import Happstack.Lite(ServerPart, ToMessage(..), lookText, Response(..), ok)
import Data.Aeson(ToJSON, encode)
import Numeric (readFloat)  -- num util
import qualified Data.ByteString.Char8 as B  -- + templates
import qualified Data.Text.Lazy as T  -- req, serialize
  
lookString :: String -> ServerPart String
lookString = fmap T.unpack . lookText

lookRead :: Read a => String -> ServerPart a
lookRead = fmap read . lookString

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