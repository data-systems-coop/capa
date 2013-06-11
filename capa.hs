{-# Language OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, 
    	     TemplateHaskell, TypeFamilies #-}
module Main (main) where
import Happstack.Lite
import Text.Blaze.Html5 (html, p, toHtml)
import Blaze.ByteString.Builder (toByteString)
import Heist (loadTemplates, HeistConfig(..), HeistState, initHeist,
              defaultLoadTimeSplices, defaultInterpretedSplices)
import Heist.Interpreted (renderTemplate)
import Control.Monad.Trans.Either
import Control.Monad.Identity
import Data.Text.Lazy (unpack)
import qualified Data.Text as DT
import Data.Monoid
import Data.Ratio ((%))
--import Data.Default 
import qualified Data.Map as M
import Data.IORef
import Control.Exception    ( bracket )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Data.Acid            ( AcidState, Query, Update, makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.Aeson.Generic as AG
import qualified Data.List as L
import Numeric (readFloat)
import qualified Data.ByteString.Char8 as B
import Data.Time (Day, fromGregorian , toGregorian, UTCTime(..), getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import qualified Data.Vector as V
import Data.Attoparsec.Number as AN

-----------------TYPES-------------------------------
data WorkPatronage = WorkPatronage {
  work::Integer, 
  skillWeightedWork::Integer,
  seniority::Integer,
  quality::Integer,
  revenueGenerated::Integer,
  performedOver::FiscalPeriod
} deriving (Show, Eq, Ord, Data, Typeable)

data PatronageWeights = PatronageWeights {
  workw::Rational,
  skillWeightedWorkw::Rational,
  seniorityw::Rational,
  qualityw::Rational,
  revenueGeneratedw::Rational
} deriving (Show, Eq, Ord, Data, Typeable)

data MemberEquityAction = MemberEquityAction {
  actionType::EquityActionType,
  amount::Integer,
  performedOn::Day, 
  resultOf::FiscalPeriod
} deriving (Show)

data EquityActionType = 
  BuyIn |
  AllocatePatronageRebate | 
  DistributeImmediate | 
  DistributeInstallment |
  EarnInterest |
  DistributeOnDeparture |
  DistributeOnDissolution |
  DistributeMilestone |
  AllocateDelayedNonQualified
   deriving (Show, Read, Data, Typeable)  
     
data Member = Member {
  firstName::String
} deriving (Show, Eq, Ord, Data, Typeable)

data FinancialResults = FinancialResults { 
  over::FiscalPeriod,
  surplus::Integer
} deriving (Show)

data FiscalPeriod = FiscalPeriod {
  start::GregorianMonth,
  periodType::PeriodType
} deriving (Show, Read, Eq, Ord, Data, Typeable)
data GregorianMonth = GregorianMonth Year Month
  deriving (Show, Read, Eq, Ord, Data, Typeable)
type Year = Integer
type Month = Int
data PeriodType = Year | Quarter
  deriving (Show, Read, Eq, Ord, Data, Typeable)

-----------------------UTIL-----------------------
lookString :: String -> ServerPart String
lookString = fmap unpack . lookText

lookRead :: Read a => String -> ServerPart a
lookRead = fmap read . lookString

readRational :: String -> Rational
readRational = toRational . fst . head . readFloat

------------------------CALCS---------------------
patronageTotal = 
  mconcat . 
    map   
      (\WorkPatronage{work=w, skillWeightedWork=sk, 
      		     seniority=sn, quality=q, revenueGenerated=r, performedOver=_} ->
         (Sum w, Sum sk, Sum sn, Sum q, Sum r))

divOrZero num 0 = 0
divOrZero num denom = num % denom

patronageProportions ps = 
  let (Sum tw, Sum tsk, Sum tsn, Sum tq, Sum tr) = patronageTotal $ M.elems ps
  in M.map 
     (\WorkPatronage{work=w,skillWeightedWork=sk,seniority=sn,
		     quality=q, revenueGenerated=r, performedOver=_} -> 
          (divOrZero w tw, divOrZero sk tsk, 
	   divOrZero sn tsn, divOrZero q tq, 
	   divOrZero r tr))
     ps

patronageAllocateRatios
  PatronageWeights{workw=ww, skillWeightedWorkw=skw, seniorityw=snw, 
  	           qualityw=qw, revenueGeneratedw=rw} = 
     M.map 
       (fromRational
       . (\(w, sk, sn, q, r) -> sum [w * ww,sk * skw,sn * snw,q * qw,r * rw]))
     . patronageProportions


allocateEquityFor FinancialResults{over=ov,surplus=sr} ps pw performedOn = 
  let memberRatios = patronageAllocateRatios pw ps
  in  
    M.map 
      (\alloc -> 
         MemberEquityAction{actionType=AllocatePatronageRebate,amount=alloc,
    	            performedOn=performedOn,resultOf=ov}) $    	       
    M.map (\proportion -> round $ proportion * toRational sr) $
    memberRatios

allMemberEquity = 
  M.map (sum . map amount)



------------------PERSIST---------------------------
data Globals = Globals { 
  members :: [Member],
  calcMethod :: Maybe (String, PatronageWeights),
  patronage :: M.Map Member WorkPatronage
} deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Globals)
$(deriveSafeCopy 0 'base ''Member)
$(deriveSafeCopy 0 'base ''PatronageWeights)
$(deriveSafeCopy 0 'base ''WorkPatronage)
$(deriveSafeCopy 0 'base ''FinancialResults)
$(deriveSafeCopy 0 'base ''FiscalPeriod)
$(deriveSafeCopy 0 'base ''GregorianMonth)
$(deriveSafeCopy 0 'base ''PeriodType)

putIt :: Globals -> Update Globals Globals
putIt g = 
  do --g@Globals{members=ms,calcMethod=m,patronage=p} <- get
     --let newMem = Member $ show n
     --put $ g{members = ms ++ [newMem]}
     put g
     return g

getIt :: Query Globals Globals
getIt = 
  ask

$(makeAcidic ''Globals ['putIt, 'getIt])

--postgresql db


--------------SERIALIZE------------------------
instance ToJSON Member where
  toJSON Member{firstName=fn} = 
  	 object ["firstName" .= fn]

instance ToJSON WorkPatronage where
  toJSON WorkPatronage{work=work,skillWeightedWork=skillWeightedWork,
		       seniority=seniority,quality=quality,
		       revenueGenerated=revenueGenerated, performedOver=prf} = 
  	 object ["work" .= work, 
	 	 "skillWeightedWork" .= skillWeightedWork, 
		 "seniority" .= seniority, "quality" .= quality,
		 "revenueGenerated" .= revenueGenerated, 
		 "performedOver" .= toJSON prf]

instance ToJSON MemberEquityAction where
  toJSON MemberEquityAction{actionType=act,amount=amt,performedOn=prf,resultOf=res} = 
  	 object ["actionType" .= AG.toJSON act, 
	 	 "amount" .= amt, 
	 	 "performedOn" .= toGregorian prf, 
		 "resultOf" .= res]

instance ToJSON GregorianMonth where
  toJSON (GregorianMonth year month) = 
    	 object ["year" .= year, 
	 	 "month" .= month]

instance ToJSON FiscalPeriod where
  toJSON FiscalPeriod{start=st,periodType=pt} = 
  	 object ["start" .= toJSON st,
	  	 "periodType" .= AG.toJSON pt]


--FromParams for PatronageWeights, WorkPatronage, FinancialResults, MemberEqAct
instance FromJSON GregorianMonth where
  parseJSON (Object v) = 
     GregorianMonth <$> v .: "year" <*> v.: "month"

instance FromJSON PeriodType where
  parseJSON (String t) = pure $ read $ DT.unpack t
     
instance FromJSON FiscalPeriod where
  parseJSON (Object v) = 
     FiscalPeriod <$> v .: "start" <*> v .: "periodType"

instance FromJSON Day where
  parseJSON (Array a) = 
     fromGregorian 
       <$> withNumber "inccorect num" (\(AN.I i) -> pure i) (a V.! 0)
       <*> withNumber "inccorect num" (\(AN.I i) -> pure $ fromIntegral i) (a V.! 1)
       <*> withNumber "inccorect num" (\(AN.I i) -> pure $ fromIntegral i) (a V.! 2)

---------------SERVICE------------------------
--coopSummary :: ServerPart Response 
coopSummary ref = do 
  v <- query' ref GetIt
  addHeaderM "Access-Control-Allow-Origin" "*"
  addHeaderM "Access-Control-Allow-Methods" "GET,POST,OPTIONS,PUT"
  ok $ toResponse $ show v

putMember ref = 
  do method POST -- PUT
     firstName <- lookString "firstName"
     let member = Member firstName
     g <- query' ref GetIt
     let mems = members g
     g2 <- update' ref (PutIt g{members = mems ++ [member]})
     ok $ toResponse ()
     
getMembers ref = 
  do method GET
     g <- query' ref GetIt
     let ms = members g
     ok $ toResponse $ do 
     	encode ms

getMemberPatronage ref = 
  path $ \(idIn::String) -> dir "patronage" $ path $ \(fiscalPeriod::Integer) ->
  do method GET 
     g <- query' ref GetIt
     let ps = patronage g
     let Just m = L.find ((\i -> i == idIn). firstName) $ members g
     let Just p = M.lookup m ps
     ok $ toResponse $ do
     	encode p  

putCalcMethod ref = 
  path $ \(methodName::String) -> 
      do method POST -- PUT
      	 let lookRational = fmap readRational . lookString
     	 workw <- lookRational "workw"
     	 skillWeightedWorkw <- lookRational "skillWeightedWorkw"
     	 seniorityw <- lookRational "seniorityw"
     	 qualityw <- lookRational "qualityw"
     	 revenueGeneratedw <- lookRational "revenueGeneratedw"
     	 let pw = PatronageWeights{workw=workw, 
     	      	  skillWeightedWorkw=skillWeightedWorkw, 
     	          seniorityw = seniorityw, 
		  qualityw = qualityw, 
		  revenueGeneratedw = revenueGeneratedw}
     	 g <- query' ref GetIt
     	 g2 <- update' ref $ PutIt g{calcMethod = Just (methodName, pw)}
     	 ok $ toResponse ()

putMemberPatronage ref = 
  path $ \(idIn::String) -> dir "patronage" $ path $ \(fiscalPeriod::Integer) ->
     do method POST -- PUT 
     	work <- lookRead "work"
     	skillWeightedWork <- lookRead "skillWeightedWork"
     	seniority <- lookRead "seniority"
     	quality <- lookRead "quality"
     	revenueGenerated <- lookRead "revenueGenerated"
	performedOverStr <- lookBS "performedOver"
	liftIO $ putStrLn $ show performedOverStr
	let Just performedOver = decode performedOverStr
     	let p = WorkPatronage{work=work, 
	      	       skillWeightedWork=skillWeightedWork,
	 	       seniority=seniority, quality=quality,
		       revenueGenerated=revenueGenerated,performedOver=performedOver}
        g <- query' ref GetIt
     	let Just m = L.find ((\i -> i == idIn). firstName) $ members g
     	g2 <- update' ref (PutIt g{patronage = M.insert m p $ patronage g})
     	ok $ toResponse ()
     	
postAllocateToMembers ref = 
  do method POST
     surplus <- lookRead "surplus"
     overStr <- lookBS "over"
     let Just over = decode overStr
     let res = FinancialResults over surplus
     g <- query' ref GetIt
     let Just (name, parameters) = calcMethod g
     UTCTime{utctDay=day,utctDayTime=_} <- liftIO getCurrentTime
     let me = allocateEquityFor res (patronage g) parameters day
     ok $ toResponse $ do 
     	encode $ M.toList me

putEquityAction ref = 
  do method POST -- PUT
     actionType <- lookRead "actionType"
     amount <- lookRead "amount"
     performedOnStr <- lookBS "performedOn"
     let Just performedOn = decode performedOnStr
     resultOfStr <- lookBS "resultOf"
     let Just resultOf = decode resultOfStr
     let act = MemberEquityAction{actionType=actionType,amount=amount,
     	          performedOn=performedOn, resultOf=resultOf}
     ok $ toResponse ()


--------------APP CONTROLLER------------------------
templateResponse name hState = 
  let rendered = runIdentity $ renderTemplate hState name
  in maybe (notFound $ toResponse $ "Template not found: " ++ B.unpack name)
  	   (\(bldr,_) -> ok $ toResponse $ toByteString bldr)
	   rendered
  	

---------------ENTRY---------------------------------
--capaApp :: ServerPart Response
capaApp ref hState = 
  msum 
  [
    -- dir "control" $ dir "configure" $ templateResponse "admin" hState
    -- dir "control" $ dir "home" $ templateResponse "home" hState - link to below, list members
    -- dir "control" $ dir "member" $ dir "add" $ templateResponse "member" hState - input member
    dir "control" $ dir "member" $ dir "patronage" $ dir "add" $ templateResponse "patronage" hState
  , dir "control" $ dir "equity" $ dir "members" $ dir "allocate" $ templateResponse "allocate" hState 
  , dir "members" $ getMembers ref
  , dir "member" $ putMember ref
  , dir "surplus" $ dir "allocate" $ dir "method" $ putCalcMethod ref
  , dir "member" $ putMemberPatronage ref
  , dir "member" $ getMemberPatronage ref
  , dir "equity" $ dir "members" $ dir "allocate" $ postAllocateToMembers ref
  , dir "member" $ dir "equity" $ dir "history" $ putEquityAction ref
  , coopSummary ref]

main = do
   let g0 = Globals [] Nothing M.empty
   x <- openLocalState g0
   ehs <- runEitherT $ do 
     templateRepo <- loadTemplates "control"
     let hCfg = (HeistConfig [] (defaultInterpretedSplices ++ defaultLoadTimeSplices) [] [] templateRepo)::HeistConfig Identity
     initHeist hCfg
   either (error . concat) (serve Nothing . capaApp x) ehs 

{---------------TODO------------------
-refine types and calcs more

-persist postgres
-expand types
-expand serialize, services


-filter by coop
-authenticate + user track 
-use across different date ranges
-polish UI
-automate test(+travis), run, release, refresh. setup hosting. document.


-configuration. automate backups, restore. 
-admin UI, export method
-(sys)logging, automate monitor.
-req + spec docs/website. bugs + enhance tracker. training tutorials.
-javascript reorganize, library survey
-remote support
-creation/install and vm of prod env
-better error with dead end web paths
-meta: pricing/donations/developer and financial health monitor
-}


