{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Channel.Server.Session
  ( withServerSession
  , requireSession
  , IsLocalState, LoginPolicy
  , LoginRequired(LoginRequired)
  , LoginOptional(LoginOptional)
  , getUser
  ) where

import Web.Channel.Server (M, Session, getSession)

import           Control.Monad          (join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Proxy             (Proxy(Proxy))
import qualified Data.Serialize                 as C
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import qualified Data.Vault.Lazy                as Vault
import qualified Network.HTTP.Types.Status      as Http
import qualified Network.URI                    as Uri
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Session            as WaiSession
import qualified Network.Wai.Util               as Wai
import qualified Web.Cookie                     as Cookie
import qualified Web.OAuth2.Google              as OAuth
import qualified Web.ServerSession.Core         as Session
import qualified Web.ServerSession.Frontend.Wai as Wai


withServerSession ::
  ( MonadIO n
  , Session.Storage sto
  , Session.SessionData sto ~ Session.SessionMap
  ) => Vault.Key Session
  -> (Session.State sto -> Session.State sto)
  -> sto
  -> n (Session.State sto, Wai.Middleware)
withServerSession key opts storage = liftIO $ do
  st <- opts <$> Session.createState storage
  return . (,) st $
    WaiSession.withSession
      (Wai.sessionStore st)
      (Text.encodeUtf8 $ Session.getCookieName st)
      (Wai.createCookieTemplate st)
      key

class IsLoginPolicy policy where
  notLoggedIn :: Proxy policy
    -> OAuth.ClientKey
    -> Wai.Application
    -> Wai.Request
    -> (Wai.Response -> IO Wai.ResponseReceived)
    -> IO Wai.ResponseReceived
  type UserType policy
  readUser :: Proxy policy -> Maybe OAuth.User -> UserType policy

data LoginRequired = LoginRequired
instance IsLoginPolicy LoginRequired where
  -- Redirect to login page.
  notLoggedIn _ oauth _ _ sendResponse = sendResponse
    =<< redirect (OAuth.loginRedirect oauth)
  type UserType LoginRequired = OAuth.User
  readUser _ = maybe e id
   where
    e = error "Web.Channel.Server.Session: session var \"user\" not set"

data LoginOptional = LoginOptional
instance IsLoginPolicy LoginOptional where
  -- Continue to app.
  notLoggedIn _ _ app request sendResponse = app request sendResponse
  type UserType LoginOptional = Maybe OAuth.User
  readUser _ = id

class (IsLoginPolicy (LoginPolicy ls)) => IsLocalState ls where
  type LoginPolicy ls

requireSession :: forall ls. (IsLocalState ls)
  => Vault.Key Session
  -> OAuth.ClientKey
  -> Proxy ls
  -> Wai.Middleware
requireSession k oauth _ app request sendResponse =
  case Vault.lookup k (Wai.vault request) of
    Nothing -> error "Main.requireSession: no session."
    Just s  -> sessionGet s "user" >>= \case
      -- User is already logged in.
      Just (user :: OAuth.User) -> app request sendResponse
      -- User is not yet logged in.
      Nothing  -> case join $ lookup "code" (Wai.queryString request) of
        -- No login code present.
        Nothing   -> notLoggedIn (Proxy :: Proxy (LoginPolicy ls))
          oauth app request sendResponse
        -- Login code is there, so use it.
        Just code -> OAuth.login oauth code >>= \case
          Left errorString -> error $
            "Main.requireSession: error logging in:\n" ++ errorString
          Right user       -> do
            sessionSet s "user" user
            app request sendResponse

redirect :: Uri.URI -> IO Wai.Response
redirect uri = Wai.redirect' Http.seeOther303 [] uri

type Key
  = Text.Text

sessionSet :: (C.Serialize a) => Session -> Key -> a -> IO ()
sessionSet (_, i) k v = i k (C.encode v)

sessionGet :: (C.Serialize a) => Session -> Key -> IO (Maybe a)
sessionGet (l, _) k = fmap (either e id . C.decode) <$> l k where
  e = error . ("Main.sessionGet: decoding failed:\n" ++)

getUser :: forall ls e. (IsLocalState ls)
  => M ls e (UserType (LoginPolicy ls))
getUser = do
  s <- getSession
  maybeUser <- liftIO $ sessionGet s "user"
  let userType = readUser (Proxy :: Proxy (LoginPolicy ls)) maybeUser
  return userType
