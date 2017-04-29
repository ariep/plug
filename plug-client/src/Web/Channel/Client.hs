{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Channel.Client
  ( runC
  , sendMany
  -- , sendManyTag
  , sendManyReceipt
  , sendManyReceiptTag
  , sendOnce
  , sendOnceReceipt
  , sendOnceReceiptTag
  , sendOnce_
  , sendFile
  , sendFileReceipt
  , get
  , getMany
  , getDyn
  ) where

import Web.Channel (ChannelID, showLabel, Channel(..), File)
import Web.Widgets (C, Make, Address, Tag, runC')

import           Control.Concurrent          (modifyMVar, modifyMVar_)
import qualified Control.Coroutine.Monadic   as Co
import           Control.Coroutine.Monadic   (Session, Eps, (:!:), (:?:), (:?*), (:&:))
import           Control.Monad.Exception     (MonadException, onException)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (runReaderT, ask)
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Base64      as B64
import           Data.Functor.Misc           (Const2(Const2))
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


runC :: (MonadWidget t m, MonadException m) => Bool -> Text.Text -> C t m a -> m a
runC debug server page = do
  ws <- WS.connect debug server
    `onException` (text "Error: the server could not be contacted.")
  let selector = fanMap . fmap (uncurry Map.singleton . messageRead) $
        WS.receiveMessages ws
  runC' (ws, selector) page 

messageRead :: WS.Message -> (Address, Text.Text)
messageRead (WS.TextMessage t) = readAddress t
  -- let (a', m) = Text.breakOn (Text.pack ":") $ t
  --     a = Text.split (== '.') a'
  -- in (a, Text.drop 1 m)

readAddress :: Text.Text -> (Address, Text.Text)
readAddress bs = let
  (i', message') = Text.break (== ':') bs
  (i, tag) = Text.break (== ';') i'
  a = Text.split (== '.') i
  mTag = if Text.null tag then Nothing else Just $ Text.drop 1 tag
  message = Text.drop 1 message'
 in
  ((a, mTag), message)

messageDecode :: (SafeCopy a) => Text.Text -> a
messageDecode t = either (e t) id .
  C.runGet safeGet . B64.decodeLenient . Text.encodeUtf8 $ t
 where
  e t m = error $ "Web.Channel.Client.messageDecode: deserialize error:\n"
    ++ show m ++ "\n"
    ++ show (B64.decodeLenient . Text.encodeUtf8 $ t)

messageEncode :: (SafeCopy a) => Channel s -> Maybe Tag -> a -> WS.Message
messageEncode (Channel i) mTag =
  WS.TextMessage .
  label i .
  Text.decodeUtf8 .
  B64.encode .
  C.runPut . safePut
 where
  label = mappend . (<> ":") . tag . Text.pack . showLabel
  tag :: Text.Text -> Text.Text
  tag = maybe id (\ t a -> a <> ";" <> t) mTag

-- | @sendMany c e@ sends the events represented by @e@ through the
-- channel @c@. Returns the events of success/failure.
sendMany :: (MonadWidget t m, SafeCopy a) =>
  Channel (Co.Repeat (a :?: Co.Eps)) -> Event t a ->
  C t m (Event t ())
sendMany c = sendMany_ c Nothing

-- sendManyTag :: (MonadWidget t m, SafeCopy a) =>
--   Channel (Co.Repeat (a :?: Co.Eps)) -> Tag -> Event t a ->
--   C t m (Event t ())
-- sendManyTag c tag = sendMany_ c (Just tag)

sendManyReceipt :: (MonadWidget t m, SafeCopy a, SafeCopy b) =>
  Channel (Co.Repeat (a :?: (b :!: Co.Eps))) -> Event t a ->
  C t m (Event t b)
sendManyReceipt c e = do
  sendMany_ c Nothing e
  get_ c Nothing

sendManyReceiptTag :: (MonadWidget t m, SafeCopy a, SafeCopy b) =>
  Channel (Co.Repeat (a :?: (b :!: Co.Eps))) -> Tag -> Event t a ->
  C t m (Event t b)
sendManyReceiptTag c tag e = do
  sendMany_ c (Just tag) e
  get_ c (Just tag)

sendOnce :: (MonadIO m, SafeCopy a) =>
  Channel (Co.Repeat (a :?: Co.Eps)) -> a ->
  C t m ()
sendOnce c x = sendOnce_ c Nothing x

sendOnce_ :: (MonadIO m, SafeCopy a) =>
  Channel (Co.Repeat (a :?: s)) -> Maybe Tag -> a ->
  C t m ()
sendOnce_ c mTag x = WS.sendMessage (messageEncode c mTag x) . fst =<< ask

sendOnceReceipt :: (MonadWidget t m, SafeCopy a, SafeCopy b) =>
  Channel (Co.Repeat (a :?: (b :!: Co.Eps))) -> a ->
  C t m (Event t b)
sendOnceReceipt c x = do
  sendOnce_ c Nothing x
  get_ c Nothing
  -- Should be better, but doesn't work yet without use of session tags.
  -- headE =<< get_ c

sendOnceReceiptTag :: (MonadWidget t m, SafeCopy a, SafeCopy b) =>
  Channel (Co.Repeat (a :?: (b :!: Co.Eps))) -> Tag -> a ->
  C t m (Event t b)
sendOnceReceiptTag c tag x = do
  sendOnce_ c (Just tag) x
  get_ c (Just tag)

sendMany_ :: (MonadWidget t m, SafeCopy a) =>
  Channel (Co.Repeat s) -> Maybe Tag -> Event t a ->
  C t m (Event t ())
sendMany_ c mTag e = WS.sendMessages (messageEncode c mTag <$> e) . fst =<<
  ask

sendFile :: forall t m. (MonadWidget t m, MonadIO (PushM t)) =>
  Channel (Co.Repeat (File :?: Co.Eps)) ->
  Event t Blob -> C t m (Event t ())
sendFile = _sendFile

_sendFile :: forall t m s. (MonadWidget t m, MonadIO (PushM t)) =>
  Channel s -> Event t Blob -> C t m (Event t ())
_sendFile (Channel i) b = do
  ws <- fst <$> ask
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
  get_ c Nothing

get :: (Reflex t, Monad m, SafeCopy a)
  => Channel (a :!: Co.Eps) -> C t m (Event t a)
get c = get_ c Nothing

getMany :: (Reflex t, Monad m, SafeCopy a)
  => Channel (Co.Repeat (a :!: Co.Eps)) -> C t m (Event t a)
getMany c = get_ c Nothing

get_ :: (Reflex t, Monad m, SafeCopy a)
  => Channel s -> Maybe Tag -> C t m (Event t a)
get_ (Channel cid) mTag = fmap messageDecode . flip select (Const2 a) . snd <$> ask
 where
  a = (map (Text.pack . show) cid, mTag)

getDyn :: (MonadWidget t m, SafeCopy a)
  => Channel (Co.Repeat (a :!: Co.Eps)) -> a -> Make t m a
getDyn c startValue = holdDyn startValue =<< getMany c

