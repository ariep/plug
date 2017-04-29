{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Channel.Server
  ( withWebSocketApp

  , ServedChannel(ServeChannel)
  , Config(..)
  , application

  , M
  , Session
  , getSession
  , getLocalState
  , ConnectionID
  , getConnectionID
  , except
  ) where

import Web.Channel

import qualified Control.Concurrent              as Conc
import           Control.Concurrent.MVar   (MVar, newMVar, readMVar, modifyMVar_, modifyMVar)
import qualified Control.Concurrent.Thread.Group as Threads
import qualified Control.Coroutine.Monadic       as Co
import           Control.Coroutine.Monadic ((:?:), (:!:), (:&:), (:++:), (:?*))
import           Control.Exception         (Exception, SomeException, bracket, finally, catch, throwIO)
import           Control.Monad             (forever, join)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (ReaderT(ReaderT), runReaderT)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import qualified Data.ByteString.Base64.Lazy     as B64
import qualified Data.ByteString.Char8           as BS8
import qualified Data.ByteString.Lazy.Char8      as BSL8
import qualified Data.ByteString.Lazy            as BSL
import           Data.Foldable             (for_)
import qualified Data.Map                        as Map
import           Data.Monoid               ((<>))
import           Data.SafeCopy             (SafeCopy, safePut, safeGet)
import qualified Data.Serialize                  as C
import qualified Data.Serialize.Get              as C
import qualified Data.Serialize.Put              as C
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import           Data.Typeable             (Typeable)
import qualified Data.Vault.Lazy                 as Vault
import qualified Network.URI                     as Uri
import qualified Network.Wai                     as Wai
import qualified Network.Wai.Handler.WebSockets  as WaiWS
import qualified Network.Wai.Session             as Wai
import qualified Network.WebSockets              as WS
import qualified Web.Cookie                      as Cookie
import qualified Web.OAuth2.Google               as OAuth
import qualified Web.ServerSession.Backend.Acid  as Acid
import qualified Web.ServerSession.Core          as Session
import qualified Web.ServerSession.Frontend.Wai  as Wai

import System.Random (randomIO)

withWebSocketApp :: Wai.Application -> WS.ServerApp -> Wai.Application
withWebSocketApp staticApp app = WaiWS.websocketsOr
  WS.defaultConnectionOptions app staticApp

data Config ls
  = Config
    {
      sessionState  :: Wai.State (Acid.AcidStorage (Session.SessionMap))
    , domain        :: Uri.URI
    , newLocalState :: IO ls
    }

application :: (Show e) => Config ls -> [ServedChannel ls e] ->
  IO WS.ServerApp
application conf channels = do
  cons <- newConnectionsState
  return $ \ pending -> do
    let headers = WS.requestHeaders $ WS.pendingRequest pending
        sessionCookie = join .
          fmap (lookup .
            BS8.pack . Text.unpack .
            Session.getCookieName . sessionState $ conf) .
          fmap Cookie.parseCookies .
          lookup "Cookie" $ headers
    originCheck (domain conf) headers (reject pending) $ bracket
      (Wai.sessionStore (sessionState conf) sessionCookie)
      (\ (_, save) -> save) -- Does not set new cookie value.
        $ \ (s, _) -> do
          ws <- WS.acceptRequest pending
          WS.forkPingThread ws 5
          conInfo <- newConnection cons
          runChannels (ws, conInfo) s (newLocalState conf) channels
 where
  reject pending reason = do
    putStrLn reason
    WS.rejectRequest pending . BS8.pack $ reason
  -- originCheck :: URI -> [(CI BS8.ByteString, BS8.ByteString)]
  --   -> (String -> IO ()) -> IO () -> IO ()
  originCheck domain headers reject accept = case lookup "Origin" headers of
    Nothing -> reject "No origin header present."
    Just o  -> case Uri.parseAbsoluteURI $ BS8.unpack o of
      Nothing -> reject "Origin fails to parse as URI."
      Just u  -> case Uri.uriAuthority u of
        Nothing -> reject "Origin does not contain authority part of URI."
        Just a | Just (Uri.uriRegName a) /= reg
                -> reject "Origin check failed."
        _       -> accept
   where
    auth = Uri.uriAuthority domain
    reg = fmap Uri.uriRegName auth

-- Channels

type Label
  = String

type Tag
  = BSL.ByteString

type ServerChan
  = (Conc.Chan (Maybe Tag, BSL.ByteString), Conc.ThreadId, MVar (Maybe Tag))

type Connection
  = (WS.Connection, ConnectionID)

type ConnectionID
  = Int

type ConnectionsState
  = MVar Int -- counter

newConnectionsState :: IO ConnectionsState
newConnectionsState = newMVar 0

newConnection :: ConnectionsState -> IO ConnectionID
newConnection cons = modifyMVar cons (\ i -> return (succ i, i))

data WebsocketClosed
  = WebsocketClosed
  deriving (Show, Typeable)

instance Exception WebsocketClosed

receiveMessages :: Connection -> Map.Map Label ServerChan -> IO ()
receiveMessages (ws, _si) channels = do
  binaryLock <- Conc.newEmptyMVar
  forever $ do
    WS.receiveDataMessage ws >>= \case
      WS.Binary bs -> (() <$) . Conc.forkIO $ do
        i <- Conc.takeMVar binaryLock
        case Map.lookup (BSL8.unpack $ fst i) channels of
          Just (c, _, _) -> Conc.writeChan c (snd i, bs)
          Nothing        -> e i
      WS.Text bs
        | BSL.take 1 bs == BSL8.pack "?" -> do
          let rest = BSL.drop 1 bs
              (i, serial) = readAddress rest
          (() <$) . Conc.forkIO $ do
            Conc.putMVar binaryLock i
            WS.sendTextData ws $ BSL8.pack "!" <> serial
      WS.Text bs   -> do
        let (i, message) = readAddress bs
        case Map.lookup (BSL8.unpack $ fst i) channels of
          Just (c, _, _) -> Conc.writeChan c . (,) (snd i) $
            B64.decodeLenient message
          Nothing        -> e i
 where
  e i = error $ "channel not found for label: " ++ show i

type Address
  = (BSL.ByteString, Maybe Tag)

readAddress :: BSL.ByteString -> (Address, BSL.ByteString)
readAddress bs = let
  (i', message') = BSL8.break (== ':') bs
  (i, tag) = BSL8.break (== ';') i'
  mTag = if BSL8.null tag then Nothing else Just $ BSL8.drop 1 tag
  message = BSL8.drop 1 message'
 in
  ((i, mTag), message)

receiveSerialized :: (SafeCopy a) => ServerChan -> IO a
receiveSerialized (chan, _, tagM) = (either e id . C.runGetLazy safeGet <$>) $ do
  (mTag, message) <- Conc.readChan chan
  modifyMVar_ tagM (const $ return mTag)
  return message
 where
  e = error . (++) "Decoding error: "

receiveByteString :: ServerChan -> IO BSL.ByteString
receiveByteString (chan, _, _) = snd <$> Conc.readChan chan

sendSerialized :: (SafeCopy a) => Connection -> Label -> MVar (Maybe Tag) -> a -> IO ()
sendSerialized (ws, _si) l tagM x = do
  mTag <- readMVar tagM
  WS.sendTextData ws . label mTag l . B64.encode . C.runPutLazy . safePut $ x
 where
  label mTag = mappend  . (<> ":"). tag mTag . BSL8.pack
  tag = maybe id (\ t a -> a <> ";" <> t)


data ServedChannel ls e where
  ServeChannel :: (ServerCoroutine (M ls e) s) =>
    Channel s -> Co.Session (M ls e) s Co.Eps () -> ServedChannel ls e

runChannels :: (Show e) =>
  Connection -> Session -> IO ls -> [ServedChannel ls e] -> IO ()
runChannels cn session newLs scs = do
  tg <- Threads.new
  ls <- newLs
  channelMap <- Map.fromList <$> mapM (runChannel tg cn session ls) scs
  receiveMessages cn channelMap
    `finally` cleanUp channelMap tg
 where
  cleanUp channelMap tg = do
    for_ (Map.elems channelMap) $ \ (_, threadId, _) ->
      Conc.throwTo threadId WebsocketClosed
    Threads.wait tg
    -- putStrLn $ "Waited for all channels to finish, shutting down connection."

runChannel :: (Show e) =>
  Threads.ThreadGroup -> Connection -> Session -> ls ->
  ServedChannel ls e -> IO (Label, ServerChan)
runChannel tg cn session ls (ServeChannel (Channel i) s) = do
  chan <- Conc.newChan
  let l = showLabel i
  tagM <- newMVar Nothing
  (threadId, _wait) <- Threads.forkIO tg . showExceptions . showExcept .
    runM cn session l tagM ls chan . serve $ Co.runSession s
  return (l, (chan, threadId, tagM))
 where
  showExceptions :: IO () -> IO ()
  showExceptions = flip catch $ \ (e :: SomeException) ->
    print e >> throwIO e
  showExcept :: (Show e) => IO (Either e ()) -> IO ()
  showExcept h = h >>= \case
    Left e   -> print e >> return ()
    Right () -> return ()

type Session
  = Wai.Session IO Text.Text BS8.ByteString

newtype M ls e a
  = M
    (ReaderT (Connection, Session, Label, ServerChan, ls)
      (ExceptT e IO)
    a)
  deriving (Functor, Applicative, Monad, MonadIO)

runM :: Connection -> Session -> Label -> MVar (Maybe Tag) -> ls ->
  Conc.Chan (Maybe Tag, BSL.ByteString) -> M ls e a -> IO (Either e a)
runM cn session i tagM ls chan (M h) = do
  threadId <- Conc.myThreadId
  runExceptT $ runReaderT h (cn, session, i, (chan, threadId, tagM), ls)
  -- TODO: signal any errors to the client here.

except :: e -> M ls e a
except = M . lift . throwE

getSession :: M ls e Session
getSession = M . ReaderT $ \ (_, s, _, _, _) -> return s

get :: (SafeCopy a) => M ls e a
get = M . ReaderT $ \ (_, _, _, chan, _) -> lift $ receiveSerialized chan

getFile :: M ls e File
getFile = M . ReaderT $ \ (_, _, _, chan, _) -> lift $ File <$> receiveByteString chan

put :: (SafeCopy a) => a -> M ls e ()
put a = M . ReaderT $ \ (cn, _, l, (_, _, tagM), _) -> lift $ sendSerialized cn l tagM a

getLocalState :: M ls e ls
getLocalState = M . ReaderT $ \ (_, _, _, _, ls) -> return ls

getConnectionID :: M ls e ConnectionID
getConnectionID = M . ReaderT $ \ ((_ws, ci), _, _, _, _) -> return ci

class ServerCoroutine m s where
  serve :: Co.InSession m s a -> m a

-- instance (Monad m) => ServerCoroutine m Co.Eps where
--   serve (Co.Eps v) = v

instance ServerCoroutine (M ls e) Co.Eps where
  serve (Co.Eps v) = v

instance {-# OVERLAPPING #-} (ServerCoroutine (M ls e) s)
  => ServerCoroutine (M ls e) (File :?: s) where
  serve (Co.R f) = serve =<< f =<< getFile

-- instance {-# OVERLAPPABLE #-} (ServerCoroutine M s, SafeCopy a)
instance {-# INCOHERENT #-} (ServerCoroutine (M ls e) s, SafeCopy a)
  => ServerCoroutine (M ls e) (a :?: s) where
  serve (Co.R f) = serve =<< f =<< get

instance (ServerCoroutine (M ls e) s, SafeCopy a)
  => ServerCoroutine (M ls e) (a :!: s) where
  serve (Co.W h) = h >>= \ (a, s) -> put a >> serve s

instance (ServerCoroutine (M ls e) s1, ServerCoroutine (M ls e) s2)
  => ServerCoroutine (M ls e) (s1 :&: s2) where
  serve (Co.O h) = h >>= \ (s1, s2) -> get >>= \case
    False -> serve s1
    True  -> serve s2

instance (ServerCoroutine m s1, ServerCoroutine m s2, Monad m)
  => ServerCoroutine m (s1 :++: s2) where
  serve (Co.CAT h) = h >>= \ c -> case c of
    Co.Cat s f -> serve s >>= serve . f

instance (ServerCoroutine m s, Monad m)
  => ServerCoroutine m (Co.Repeat s) where
  serve (Co.Repeat s) = serve s

instance (ServerCoroutine (M ls e) s, ServerCoroutine (M ls e) r)
  => ServerCoroutine (M ls e) (s :?* r) where
  serve (Co.StarS s) = serve s
