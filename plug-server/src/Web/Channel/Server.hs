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
  ) where

import Web.Channel

import qualified Control.Concurrent              as Conc
import qualified Control.Concurrent.Thread.Group as Threads
import qualified Control.Coroutine.Monadic       as Co
import           Control.Coroutine.Monadic ((:?:), (:!:), (:&:), (:++:), (:?*))
import           Control.Exception         (Exception, bracket, finally)
import           Control.Monad             (forever, join)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (ReaderT(ReaderT), runReaderT)
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

data Config
  = Config
    {
      sessionState :: Wai.State (Acid.AcidStorage (Session.SessionMap))
    , domain       :: Uri.URI
    }

application :: Config -> [ServedChannel] ->
  WS.ServerApp
application conf channels pending = do
  let headers = WS.requestHeaders $ WS.pendingRequest pending
      sessionCookie = join .
        fmap (lookup .
          BS8.pack . Text.unpack .
          Session.getCookieName . sessionState $ conf) .
        fmap Cookie.parseCookies .
        lookup "Cookie" $ headers
  originCheck (domain conf) headers reject $ bracket
    (Wai.sessionStore (sessionState conf) sessionCookie)
    (\ (_, save) -> save) -- Does not set new cookie value.
      $ \ (s, _) -> do
        ws <- WS.acceptRequest pending
        WS.forkPingThread ws 5
        si <- randomIO
        runChannels (ws, si) s channels
 where
  reject reason = do
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

type ServerChan
  = (Conc.Chan (BSL.ByteString), Conc.ThreadId)

type Connection
  = (WS.Connection, Int)

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
        case Map.lookup (BSL8.unpack i) channels of
          Just (c, _) -> Conc.writeChan c bs
          Nothing     -> e i
      WS.Text bs
        | BSL.take 1 bs == BSL8.pack "?" -> do
          let rest = BSL.drop 1 bs
              (i, serial') = BSL8.break (== ':') rest
              serial = BSL8.drop 1 serial'
          (() <$) . Conc.forkIO $ do
            Conc.putMVar binaryLock i
            WS.sendTextData ws $ BSL8.pack "!" <> serial
      WS.Text bs   -> do
        let (i, message') = BSL8.break (== ':') bs
            message = BSL8.drop 1 message'
        case Map.lookup (BSL8.unpack i) channels of
          Just (c, _) -> Conc.writeChan c $
            B64.decodeLenient message
          Nothing     -> e i
 where
  e i = error $ "channel not found for label: " ++ show i

receiveSerialized :: (SafeCopy a) => ServerChan -> IO a
receiveSerialized (chan, _) = either e id . C.runGetLazy safeGet <$>
  Conc.readChan chan
 where
  e = error . (++) "Decoding error: "

receiveByteString :: ServerChan -> IO BSL.ByteString
receiveByteString (chan, _) = Conc.readChan chan

sendSerialized :: (SafeCopy a) => Connection -> Label -> a -> IO ()
sendSerialized (ws, si) l = WS.sendTextData ws .
  label l . B64.encode . C.runPutLazy . safePut
 where
  label = mappend . BSL8.pack . (++ ":")


data ServedChannel where
  ServeChannel :: (ServerCoroutine M s) =>
    Channel s -> Co.Session M s Co.Eps () -> ServedChannel

runChannels :: Connection -> Session -> [ServedChannel] -> IO ()
runChannels cn session scs = do
  tg <- Threads.new
  channelMap <- Map.fromList <$> mapM (runChannel tg cn session) scs
  receiveMessages cn channelMap
    `finally` cleanUp channelMap tg
 where
  cleanUp channelMap tg = do
    for_ (Map.elems channelMap) $ \ (_, threadId) ->
      Conc.throwTo threadId WebsocketClosed
    Threads.wait tg
    -- putStrLn $ "Waited for all channels to finish, shutting down connection "
    --   ++ show (snd cn)

runChannel :: Threads.ThreadGroup -> Connection -> Session ->
  ServedChannel -> IO (Label, ServerChan)
runChannel tg cn session (ServeChannel (Channel i) s) = do
  chan <- Conc.newChan
  let l = showLabel i
  (threadId, _wait) <- Threads.forkIO tg . runM cn session l chan $ do
    serve $ Co.runSession s
  return (l, (chan, threadId))

type Session
  = Wai.Session IO Text.Text BS8.ByteString

newtype M a
  = M (ReaderT (Connection, Session, Label, ServerChan) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runM :: Connection -> Session -> Label -> Conc.Chan (BSL.ByteString) ->
  M a -> IO a
runM cn session i chan (M h) = do
  threadId <- Conc.myThreadId
  runReaderT h (cn, session, i, (chan, threadId))

getSession :: M Session
getSession = M . ReaderT $ \ (_, s, _, _) -> return s

get :: (SafeCopy a) => M a
get = M . ReaderT $ \ (_, _, _, chan) -> receiveSerialized chan

getFile :: M File
getFile = M . ReaderT $ \ (_, _, _, chan) -> File <$> receiveByteString chan

put :: (SafeCopy a) => a -> M ()
put a = M . ReaderT $ \ (cn, _, l, _) -> sendSerialized cn l a

class ServerCoroutine m s where
  serve :: Co.InSession m s a -> m a

-- instance (Monad m) => ServerCoroutine m Co.Eps where
--   serve (Co.Eps v) = v

instance ServerCoroutine M Co.Eps where
  serve (Co.Eps v) = v

instance {-# OVERLAPPING #-} (ServerCoroutine M s)
  => ServerCoroutine M (File :?: s) where
  serve (Co.R f) = serve =<< f =<< getFile

-- instance {-# OVERLAPPABLE #-} (ServerCoroutine M s, SafeCopy a)
instance {-# INCOHERENT #-} (ServerCoroutine M s, SafeCopy a)
  => ServerCoroutine M (a :?: s) where
  serve (Co.R f) = serve =<< f =<< get

instance (ServerCoroutine M s, SafeCopy a)
  => ServerCoroutine M (a :!: s) where
  serve (Co.W h) = h >>= \ (a, s) -> put a >> serve s

instance (ServerCoroutine M s1, ServerCoroutine M s2)
  => ServerCoroutine M (s1 :&: s2) where
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

instance (ServerCoroutine M s, ServerCoroutine M r)
  => ServerCoroutine M (s :?* r) where
  serve (Co.StarS s) = serve s
