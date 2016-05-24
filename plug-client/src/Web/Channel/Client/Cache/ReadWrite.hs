{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Channel.Client.Cache.ReadWrite
  ( Cache
  , createEmpty
  , interestedEvent
  , interestedNow
  , update
  , current
  ) where

import Common

import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar   (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM    (STM, atomically, retry)
import           Control.Coroutine.Monadic (Repeat, Eps, (:!:), (:?:))
import           Control.Exception         (SomeException, throw, catch)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Functor              (void)
import           Data.Hashable             (Hashable)
import           Data.SafeCopy             (SafeCopy)
import           Focus                     (StrategyM, Decision(Keep, Replace))
import           GHC.Conc.Sync             (unsafeIOToSTM)
import           Reflex                    (Dynamic, Event, holdDyn, leftmost, ffilter)
import           Reflex.Dom                (MonadWidget)
import qualified STMContainers.Map        as SMap
import qualified Web.Channel              as Ch
import qualified Web.Channel.Client       as Ch
import qualified Web.Channel.Client.Cache as Cache
import           Web.Widgets               (C, Make, Address, Tag, asyncEvent)


data Cache t m id a
  = Cache
    { items   :: SMap.Map id (Either Pending (Dynamic t (Maybe a)))
    , remotes :: Event t [a]
    , locals  :: Event t a
    , trigger :: a -> C t m ()
    }

createEmpty :: (SafeCopy a, MonadWidget t m) =>
  Ch.Channel (Ch.Change [a]) -> C t m (Cache t m id a)
createEmpty cc = do
  smap <- liftIO SMap.newIO
  remote <- Ch.getMany cc
  (local, u) <- asyncEvent
  return $ Cache smap remote local u

data Pending
  = Pending

interestedEvent :: (MonadWidget t m, SafeCopy id) =>
  Ch.Channel (Ch.Interest id) -> Event t Bool -> id ->
  C t m ()
interestedEvent ic ev i = void $ Ch.sendMany ic ((,) i <$> ev)

interestedNow :: (MonadWidget t m, SafeCopy id) =>
  Ch.Channel (Ch.Interest id) -> id ->
  C t m ()
interestedNow ic i = Ch.sendOnce ic (i, True)


current :: (Hashable id, Eq id, MonadWidget t m) =>
  Cache t m id a -> (a -> id) -> id ->
  C t m (Dynamic t (Maybe a))
current cache relevant item = do
  let u :: StrategyM STM
        (Either Pending (Dynamic t (Maybe a)))
        (Maybe (Either Pending (Dynamic t (Maybe a))))
      u = \case
        Nothing -> return (Nothing, Replace $ Left Pending)
        r       -> return (r      , Keep)
  liftIO (atomically $ SMap.focus u item $ items cache) >>= \case
    Nothing -> do
    -- TODO: design a better way to deal with multiple changes.
      let changeE = leftmost . (locals cache :) . return . fmap head $
            ffilter (not . null . filter ((== item) . relevant)) $ remotes cache
      resultDyn <- holdDyn Nothing $ Just <$> changeE
      liftIO . atomically $ SMap.insert (Right resultDyn) item $ items cache
      return resultDyn
    Just (Left Pending) -> liftIO . atomically $ do
      SMap.lookup item (items cache) >>= \case
        Just (Left Pending) -> do
          safeIOToSTM $ putStrLn "Web.Channel.Client.Cache.ReadWrite.current: retrying."
          retry
        Just (Right d)      -> return d
    Just (Right d)      -> return d

update :: Cache t m id a -> a -> C t m ()
update cache a = trigger cache a

