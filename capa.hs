{-# Language OverloadedStrings #-}
module Main (main) where

import Types
import Utils
import Domain
import Serialize
import Persist
import Service

import Happstack.Lite
   (ok, method, serve, dir, Method(..), nullDir, notFound, ToMessage(..), seeOther)
import Happstack.Server.Routing (dirs)

import Network.HTTP.Conduit(simpleHttp)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Generic as AG 
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Aeson.Types as AT
import Data.Acid.Advanced (query')

import Text.Blaze.Html5 (html, p, toHtml) -- present, html, temlate
import Blaze.ByteString.Builder (toByteString)
import qualified Data.ByteString.Char8 as B  -- + templates
import Heist (loadTemplates, HeistConfig(..), HeistState, initHeist,
              defaultLoadTimeSplices, defaultInterpretedSplices)
import Heist.Interpreted (renderTemplate)
import Control.Monad.Trans.Either
import Control.Monad.Identity

import Control.Exception(bracket)     -- util
import Data.Acid(openLocalState)


--------------APP CONTROLLER------------------------
type TemplateStore = HeistState Identity
                     
--expose default templateResponse with check==True                     
                     
templateResponse name hState =  -- parameter to check cookie or not
  nullDir >> method GET >>
  --if check, then check cookie is set
  let rendered = runIdentity $ renderTemplate hState name
  in maybe (notFound $ toResponse $ "Template not found: " ++ B.unpack name)
  	   (\(bldr,_) -> ok $ toResponse $ toByteString bldr)
	   rendered  	
     

resolveCoop ref = do 
  token <- lookString "token"
  let reqUri = "https://rpxnow.com/api/v2/auth_info" ++ 
               "?" ++ "apiKey=" ++ "f3333416cb3a1af4761a6472123dd9979b9f6532"
               ++ "&" ++ "token=" ++ token
  r <- liftIO $ simpleHttp reqUri
  let Just (A.Object r2) = A.decode r
  let AT.Success pf = (AT.parse (A..: "profile") r2)::AT.Result A.Object
  let AT.Success ident = (AT.parse (A..: "identifier") pf)::AT.Result String
  Globals{cooperative=Cooperative{username=user}} <- query' ref GetIt
  let redir = if user == ident then "/control/financial/results" else "/control/enter"
  seeOther (redir::String) (toResponse ()) -- set cookie, home
    -- then redirect to login screen with "Not associated with coop"

  
---------------ENTRY---------------------------------
capaApp :: PersistConnection -> TemplateStore -> ServerPartR
capaApp ref hState = msum [
    dir "control" $ msum [
         dir "coop" $ msum [
            dir "summary" $ templateResponse "coopSummary" hState
          , dir "register" $ templateResponse "registerCoop" hState
          , dir "settings" $ templateResponse "coopSettings" hState ] 
             -- view all, update alloc, update disbursal
       , dir "member" $ msum [
            dir "accounts" $ templateResponse "memberAccounts" hState
          , dir "account" $ dir "action" $ dir "add" $ 
              templateResponse "addAction" hState
          , dir "patronage" $ dir "record" $ 
              templateResponse "recordPatronage" hState ]
       , dir "members" $ msum [
             dir "patronage" $ dir "period" $ templateResponse "periodPatronage" hState
           , dir "add" $ templateResponse "newMember" hState ]
       , dir "financial" $ dir "results" $ msum [
             templateResponse "financialResults" hState
           , dir "record" $ templateResponse "recordResult" hState ]
       , dirs "equity" $ msum [ 
            dir "members"  $ dir "allocationsDisbursals" $ 
              templateResponse "allocationsDisbursals" hState ] 
       , dir "enter" $ templateResponse "enter" hState 
       , dir "login" $ dir "resolve" $ dir "coop" $ method POST >> resolveCoop ref]
  
  , dir "financial" $ dir "results" $ msum [ 
       method GET >> getAllFinancialResultsDetail ref
     , method POST >> putFinancialResults ref ]
  , dir "members" $ msum [
        nullDir >> method GET >> getMembers ref  
      , dir "patronage" $ method GET >> getAllMemberPatronage ref ]
  , dir "member" $ msum [ 
         method POST >> putMember ref
       , method GET >> getMember ref
       , method POST >> putMemberPatronage ref
       , dir "equity" $ msum [
           dir "disburse" $ method POST >> postScheduleAllocateDisbursal ref
         , dir "history" $ method POST >> putEquityAction ref ] ]
  , dir "equity" $ msum [
       dir "members" $ msum [
         dir "allocate" $ msum [
             dir "generate" $ method POST >> postAllocateToMembers ref 
           , dir "save" $ method POST >> postAllocationDisbursal ref] ] ]
  , dir "coop" $ msum [
       dir "settings" $ msum [
          dir "allocate" $ method POST >> putCoopAllocateSettings ref] ]
  , dir "fiscal" $ dir "periods" $ getLatestFiscalPeriods ref
  , dir "dump" $ dump ref]
                     
main = do
   x <- openLocalState g0
   ehs <- runEitherT $ do 
     templateRepo <- loadTemplates "control"
     let hCfg = (HeistConfig 
	           [] 
	           (defaultInterpretedSplices ++ defaultLoadTimeSplices) 
	           [] 
	           [] 
	           templateRepo)::HeistConfig Identity
     initHeist hCfg
   either (error . concat) (serve Nothing . capaApp x) ehs 


