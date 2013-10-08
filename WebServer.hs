module WebServer
where
  
import Utils
import Happstack.Server(simpleHTTPWithSocket, nullConf, bindPort, Conf(..))
import System.Posix.User (setUserID, UserEntry(..), getUserEntryForName)
import Network.Socket

openSocket :: Int -> String -> IO Socket
openSocket port webUser = do
  socket <- bindPort nullConf { port = port }
  getUserEntryForName webUser >>= setUserID . userID
  return socket
  
serve :: Socket -> Int -> ServerPartR -> IO ()
serve socket port app = 
  simpleHTTPWithSocket socket nullConf{port = port} app
