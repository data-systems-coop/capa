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
  infoM "main" "started"
  let authUriBase = getConfig cp "authuribase"
  let servicesUri = getConfig cp "servicesuri"::String
  let connString = 
       printf "host=%s port=%d dbname=%s user=%s password=%s"
         (getConfig cp "dbhost"::String) (getConfig cp "dbport"::Integer)
         (getConfig cp "dbname"::String) (getConfig cp "dbuser"::String)
         (getConfig cp "dbpass"::String)
  let webPort = getConfig cp "webport"
  socket <- openSocket webPort $ getConfig cp "webprocessuser"  
  cache <- openLocalStateFrom "state" g0   --restart cache (add dir param)
  tmpltRepo <- fmap forceEither $ initTemplateRepo $ getConfig cp "templatedir"
  serve socket webPort $ 
     capaApp 
       cache 
       connString
       (authenticatedForRegister authUriBase)
       (resolveCoop authUriBase cache)  --provide auth handlers
       tmpltRepo

getConfig :: (CF.Get_C a) => CF.ConfigParser -> CF.OptionSpec -> a
getConfig cp = forceEither . CF.get cp "DEFAULT"

setupLogging :: String -> Priority -> IO ()
setupLogging progName rootLogLevel = do 
  s <- SYS.openlog progName [] SYS.USER DEBUG  
  updateGlobalLogger rootLoggerName (addHandler s . setLevel rootLogLevel)

main :: IO ()
main = traplogging "main" ERROR "Server quit due to" run 
