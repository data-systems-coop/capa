module View where

import Utils

import Blaze.ByteString.Builder (toByteString) -- present, temlate
import qualified Data.ByteString.Char8 as B  -- + templates
import Heist (HeistState)
import Heist.Interpreted (renderTemplate)
import Control.Monad.Identity

import Heist (loadTemplates, HeistConfig(..), initHeist,
              defaultLoadTimeSplices, defaultInterpretedSplices)
import Control.Monad.Trans.Either
import Control.Monad.Identity

import Happstack.Lite(ok, notFound, ToMessage(..))

type TemplateStore = HeistState Identity


templateFor :: TemplateStore -> B.ByteString -> ServerPartR
templateFor hState name = do
  -- fetch known template
  let rendered = runIdentity $ renderTemplate hState name
  maybe (notFound $ toResponse $ "Template not found: " ++ B.unpack name) --scrap this
        -- expand template, build response and code
  	(\(bldr,_) -> ok $ toResponse $ toByteString bldr)
	rendered  	

initTemplateRepo :: String -> IO (Either [String] (HeistState Identity))
initTemplateRepo templateDir = 
  runEitherT $ do 
   templateRepo <- loadTemplates templateDir
   initHeist ((HeistConfig 
	         [] 
	         (defaultInterpretedSplices ++ defaultLoadTimeSplices) 
	         [] 
	         [] 
	         templateRepo)::HeistConfig Identity)
