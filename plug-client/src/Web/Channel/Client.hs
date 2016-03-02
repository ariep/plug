{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Channel.Client
  ( sendMany
  , sendManyReceipt
  , sendOnce
  , sendFile
  , sendFileReceipt
  , get
  , getMany
  , getDyn
  ) where

import Web.Channel (ChannelID, showLabel, Channel(..), File)
import Web.Widgets (C, Make)

import           Control.Concurrent          (modifyMVar, modifyMVar_)
import qualified Control.Coroutine.Monadic   as Co
import           Control.Coroutine.Monadic   (Session, Eps, (:!:), (:?:), (:?*), (:&:))
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (ask)
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Base64      as B64
import qualified Data.Map                    as Map
import           Data.Monoid                 ((<>))
import           Data.SafeCopy               (SafeCopy, safePut, safeGet)
import qualified Data.Serialize              as C
import qualified Data.Serialize.Get          as C
import qualified Data.Serialize.Put          as C
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import           Data.Tuple                  (swap)
import qualified JavaScript.WebSockets.Reflex.WebSocket as WS
import           JavaScript.Web.Blob         (Blob)
import           Reflex.Dom


messageDecode :: (SafeCopy a) => Channel s -> WS.Message -> Maybe a
messageDecode (Channel i) (WS.TextMessage t) = 
  let (i', m) = Text.breakOn (Text.pack ":") $ t
  in if Text.pack (showLabel i) == i'
    then Just . either e id . C.runGet safeGet . B64.decodeLenient . Text.encodeUtf8 . Text.drop 1 $ m
    else Nothing
 where
  e = error "JavaScript.WebSockets.Reflex.WebSocket.messageToBinary: deserialize error"

messageEncode :: (SafeCopy a) => Channel s -> a -> WS.Message
messageEncode (Channel i) =
  WS.TextMessage .
  label i .
  Text.decodeUtf8 .
  B64.encode .
  C.runPut . safePut
 where
  label = mappend . Text.pack . (++ ":") . showLabel

-- | @sendMany c e@ sends the events represented by @e@ through the
-- channel @c@. Returns the events of success/failure.
sendMany :: (MonadWidget t m, SafeCopy a)
  => Channel (Co.Repeat (a :?: Co.Eps)) -> Event t a
  -> C t m (Event t ())
sendMany = sendMany_

sendManyReceipt :: (MonadWidget t m, SafeCopy a, SafeCopy b)
  => Channel (Co.Repeat (a :?: (b :!: Co.Eps))) -> Event t a
  -> C t m (Event t b)
sendManyReceipt c e = do
  sendMany_ c e
  get_ c

sendOnce :: (MonadIO m, SafeCopy a) =>
  Channel (Co.Repeat (a :?: s)) -> a ->
  C t m ()
sendOnce c x = ask >>= WS.sendMessage (messageEncode c x)

sendMany_ :: (MonadWidget t m, SafeCopy a)
  => Channel (Co.Repeat s) -> Event t a
  -> C t m (Event t ())
sendMany_ c e = ask >>= WS.sendMessages (messageEncode c <$> e)

sendFile :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => Channel (Co.Repeat (File :?: Co.Eps))
  -> Event t Blob -> C t m (Event t ())
sendFile = _sendFile

_sendFile :: forall t m s. (MonadWidget t m, MonadIO (PushM t))
  => Channel s -> Event t Blob -> C t m (Event t ())
_sendFile (Channel i) b = do
  ws <- ask
  -- Annotate blobs with a serial number.
  let e = withCounter (WS.counter ws) b
  -- First notify the server that we wish to send a binary message.
  askReady ws e
  -- Store blobs in a map until they are needed.
  performEvent_ $ flip fmap e $ \ (b, i) -> do
    liftIO . modifyMVar_ (WS.blobs ws) $ return . Map.insert i b
  -- Send the binary data when the server signals it is ready to receive.
  sendBlob ws $ flip push (ready ws) $
    \ serial -> liftIO $ modifyMVar (WS.blobs ws) $
      return . swap . Map.updateLookupWithKey
        (const $ const Nothing) -- Delete blob from map.
        serial                  -- Use serial number as index.
 where
  askReady :: WS.Connection t -> Event t (a, Text.Text) -> C t m ()
  askReady ws = fmap (const ()) . flip WS.sendMessages ws . fmap
    (\ (_, serial) -> WS.TextMessage $
      Text.pack "?" <> Text.pack (showLabel i) <> Text.pack ":" <> serial)

  ready :: WS.Connection t -> Event t Text.Text
  ready ws = flip fmapMaybe (WS.receiveMessages ws) $ \case
    WS.TextMessage t
      | Text.take 1 t == Text.pack "!" -> Just $ Text.drop 1 t
    _                                  -> Nothing

  sendBlob :: WS.Connection t -> Event t Blob -> C t m (Event t ())
  sendBlob ws = flip WS.sendMessages ws . fmap WS.BlobMessage

  withCounter :: WS.Counter -> Event t a -> Event t (a, Text.Text)
  withCounter ct = pushAlways $
    \ a -> (,) a . Text.pack . show <$> liftIO (WS.tick ct)

sendFileReceipt :: forall t m a.
  ( MonadWidget t m
  , MonadIO (PushM t)
  , SafeCopy a)
  => Channel (Co.Repeat (File :?: (a :!: Co.Eps)))
  -> Event t Blob -> C t m (Event t a)
sendFileReceipt c b = do
  _sendFile c b
  get_ c

get :: (Reflex t, Monad m, SafeCopy a)
  => Channel (a :!: Co.Eps) -> C t m (Event t a)
get c = get_ c

getMany :: (Reflex t, Monad m, SafeCopy a)
  => Channel (Co.Repeat (a :!: Co.Eps)) -> C t m (Event t a)
getMany c = get_ c

get_ :: (Reflex t, Monad m, SafeCopy a)
  => Channel s -> C t m (Event t a)
get_ c = fmapMaybe (messageDecode c) . WS.receiveMessages <$> ask

getDyn :: (MonadWidget t m, SafeCopy a)
  => Channel (Co.Repeat (a :!: Co.Eps)) -> a -> Make t m a
getDyn c startValue = holdDyn startValue =<< getMany c

