module Service.Base(
  module Service.Base,
  module Control.Monad.Reader,
  ServerPartT, Response(..), ok, toResponseBS, look, lookBS, 
  Connection,
  encode, decode,
  urlEncode
) where

import Control.Monad.IO.Class (liftIO)  
import Control.Monad.Reader
import Happstack.Server(ServerPartT, Response(..), ok, toResponseBS, look, lookBS
                       ) 
import Database.HDBC.PostgreSQL(Connection)
import Control.Monad(guard, void)
import Data.Aeson (encode, decode)
import Network.HTTP.Base(urlEncode)