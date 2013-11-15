{-# Language OverloadedStrings, ScopedTypeVariables #-}
module Main (main) where

import Utils
import Persist.Persist(Globals(..))
import Service.Service(authenticatedForRegister, resolveCoop)
import View
import WebServer
import Router

import qualified Control.Exception as EX --util

import Data.Acid(openLocalStateFrom)
import qualified Data.Map as M

import Data.Either.Utils (forceEither)
import qualified Data.ConfigFile as CF

import qualified System.Log.Handler.Syslog as SYS
import System.Environment(getArgs, getProgName)
import System.Log.Logger(traplogging, Priority)

g0 = Globals M.empty

run :: IO ()  
run = do
  ((configFile:_), prog) <- (,) <$> getArgs <*> getProgName
  cp <- fmap forceEither $ CF.readfile CF.emptyCP configFile
  setupLogging prog $ getConfig cp "rootloglevel"
  let (authUriBase, servicesUri, connString, webPort) = 
        (getCfgString cp "authuribase", 
         getCfgString cp "servicesuri", 
         printf "host=%s port=%d dbname=%s user=%s password=%s"
           (getCfgString cp "dbhost") (getConfig cp "dbport"::Integer)
           (getCfgString cp "dbname") (getCfgString cp "dbuser")
           (getCfgString cp "dbpass"),
         getConfig cp "webport")
  (socket, cache, tmpltRepo) <- 
    (,,) <$> (openSocket webPort $ getConfig cp "webprocessuser")
      <*> (openLocalStateFrom "state" g0)   --(add dir param)
      <*> (fmap forceEither $ initTemplateRepo $ getConfig cp "templatedir")
  serve socket webPort $ 
     capaApp 
       cache 
       connString
       (authenticatedForRegister authUriBase) --provide auth handlers
       (resolveCoop authUriBase cache)  
       tmpltRepo

getCfgString :: CF.ConfigParser -> CF.OptionSpec -> String
getCfgString = getConfig

getConfig :: (CF.Get_C a) => CF.ConfigParser -> CF.OptionSpec -> a
getConfig cp = forceEither . CF.get cp "DEFAULT"

setupLogging :: String -> Priority -> IO ()
setupLogging progName rootLogLevel = do 
  s <- SYS.openlog progName [] SYS.USER DEBUG  
  updateGlobalLogger rootLoggerName (addHandler s . setLevel rootLogLevel)
  infoM "Main.setupLogging" "started"

main :: IO ()
main = traplogging "Main.main" ERROR "Server quit due to" run 
