{-# Language ScopedTypeVariables #-}
module Service.Service (
  module Service.Service, 
  module Service.Cooperative, 
  module Service.Member,  
  module Service.WorkPatronage,  
  module Service.FinancialResults,
  module Service.MemberEquityAccount,
  module Service.Allocation,
  module Service.Settings,
  module Service.Admin,
  module Service.Time,  
  module Service.Security
) where

import Service.Security
import Service.Time
import Service.Cooperative
import Service.Settings
import Service.Admin
import Service.WorkPatronage
import Service.Member
import Service.FinancialResults 
import Service.MemberEquityAccount
import Service.Allocation

import Utils
import qualified Data.ByteString.Char8 as B  -- + templates
import Database.HDBC.PostgreSQL(Connection)
import Persist.Persist(PersistConnection)
import Happstack.Lite
   (method, Method(..), nullDir,
    CookieLife(Session), mkCookie, addCookies, expireCookie, lookCookieValue)
import Happstack.Server.RqData(HasRqData(..), RqEnv)

generalTemplateResponse :: 
  String -> String -> (B.ByteString -> ServerPartR) -> PersistConnection -> Bool -> 
  Bool -> B.ByteString -> Connection -> ServerPartR
generalTemplateResponse 
  loginUrl partialRegUrl templateFor ref secure checkReg name dbCn = 
   nullDir >> method GET >> do 
    -- lookup cookie for sessionid, if any
    (_, _, cookies) <- askRqEnv
    let mbSession = lookup "sessionid" cookies --lookCookieValue
    -- based on secure or not x session x check registration status
        -- either redirect to login, partially registered, or page requested
    if secure && isNothing mbSession
      then redirect loginUrl 
      else do 
        if checkReg
           then do 
             (alloc, disb) <- getCoopRegistrationState ref dbCn
             if (not alloc || not disb) 
               then
                 redirect $ printf partialRegUrl (show alloc) (show disb) 
               else
                 templateFor name  
           else
             templateFor name

