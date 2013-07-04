{-# Language OverloadedStrings #-}
module Main (main) where

import Types
import Utils
import Domain
import Serialize
import Persist
import Service

import Happstack.Lite
   (ok, method, serve, dir, Method(..), nullDir, notFound, ToMessage(..))
import Happstack.Server.Routing (dirs)

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
                     
templateResponse name hState = 
  nullDir >> method GET >>
  let rendered = runIdentity $ renderTemplate hState name
  in maybe (notFound $ toResponse $ "Template not found: " ++ B.unpack name)
  	   (\(bldr,_) -> ok $ toResponse $ toByteString bldr)
	   rendered
  	

---------------ENTRY---------------------------------
capaApp :: PersistConnection -> TemplateStore -> ServerPartR
capaApp ref hState = msum [
    dir "control" $ msum [
         dir "coop" $ msum [
            dir "summary" $ templateResponse "coopSummary" hState
          , dir "register" $ templateResponse "registerCoop" hState
          , dir "settings" $ templateResponse "coopSettings" hState ] -- view all, update alloc, update disbursal
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
              templateResponse "allocationsDisbursals" hState ] ]
  
  , dir "financial" $ dir "results" $ msum [ 
       method GET >> getAllFinancialResultsDetail ref
     , method POST >> putFinancialResults ref ]
  , dir "surplus" $ 
      dir "allocate" $ 
        dir "method" $ method POST >> putCalcMethod ref
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
           , dir "save" $ method POST >> postAllocationDisbursal ref
         ] ] ]
  , dir "fiscal" $ dir "periods" $ getLatestFiscalPeriods ref
  , coopSummary ref]
                     
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


