{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Channel.Client.Cache.ReadWrite
  ( Cache
  , create
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
import qualified Data.Change              as Change
import           Data.Functor              (void)
import           Data.Hashable             (Hashable)
import qualified Data.Map                 as Map
import           Data.Proxy                (Proxy(Proxy))
import           Data.SafeCopy             (SafeCopy)
import           Focus                     (StrategyM, Decision(Keep, Replace), adjustM)
import           GHC.Conc.Sync             (unsafeIOToSTM)
import           Reflex                    (Dynamic, Event, holdDyn, leftmost, ffilter)
import           Reflex.Dynamic            (foldDyn)
import           Reflex.Dom                (MonadWidget)
import qualified STMContainers.Map        as SMap
import qualified Web.Channel              as Ch
import qualified Web.Channel.Client       as Ch
import qualified Web.Channel.Client.Cache as Cache
import           Web.Widgets               (C, Make, Address, Tag, asyncEventIO, onEvent)


-- data Cache t m id a
--   = Cache
--     { items   :: SMap.Map id (Either Pending (Dynamic t (Maybe a)))
--     , remotes :: Event t [a]
--     , locals  :: Event t a
--     , trigger :: a -> C t m ()
--     }
data Cache t x
  = Cache
    { current  :: Dynamic t (Maybe x)
    , updateIO :: Change.Changes x -> IO ()
    }

update :: (MonadWidget t m) => Cache t x -> Event t (Change.Changes x) -> m ()
update c e = onEvent e (liftIO . updateIO c)

class (Change.Changing x) => Cachable x where
  type STMCache x
  newCache    :: Proxy x -> STM (STMCache x)
  updateCache :: Proxy x -> Change.Changes x -> STMCache x -> STM ()

instance (Change.Changing v, Ord k, Hashable k) => Cachable (Map.Map k v) where
  type STMCache (Map.Map k v) = SMap.Map k v
  newCache _ = SMap.new
  updateCache _ cs m = mapM_ (($ m) . go) cs where
    go (Change.MapAdd k v)    = SMap.insert v k
    go (Change.MapModify k c) = void . SMap.focus
      (adjustM $ return . Change.apply c) k
    go (Change.MapDelete k)   = SMap.delete k

-- createEmpty :: (SafeCopy a, MonadWidget t m) =>
--   Ch.Channel (Change.Change [a]) -> C t m (Cache t m id a)
-- createEmpty cc = do
--   smap <- liftIO SMap.newIO
--   remote <- Ch.getMany cc
--   (local, u) <- asyncEvent
--   return $ Cache smap remote local u

-- Note: waits for changes to come back via the server before they are
-- reflected in the cache.
create :: forall t m x.
  -- ( Cachable x
  ( Change.Changing x
  , SafeCopy x
  , SafeCopy (Change.Changes x)
  , MonadWidget t m
  ) =>
  Ch.Token (Ch.InitialPushPull x) -> C t m (Cache t x)
create t = do
  -- cache <- liftIO $ atomically $ newCache (Proxy :: Proxy x)
  initial <- Ch.getMany $ Ch.initial t
  change <- Ch.getMany $ Ch.pullChange t
  let allE = leftmost [Left <$> initial, Right <$> change]
  cur <- foldDyn f Nothing allE
  (localChangeE, tr) <- asyncEventIO
  Ch.sendMany (Ch.pushChange t) localChangeE
  return $ Cache cur tr
 where
  f (Left x)  _   = Just x
  f (Right c) old = fmap (Change.apply c) old

-- data Pending
--   = Pending

-- interestedEvent :: (MonadWidget t m, SafeCopy id) =>
--   Ch.Channel (Ch.Interest id) -> Event t Bool -> id ->
--   C t m ()
-- interestedEvent ic ev i = void $ Ch.sendMany ic ((,) i <$> ev)

-- interestedNow :: (MonadWidget t m, SafeCopy id) =>
--   Ch.Channel (Ch.Interest id) -> id ->
--   C t m ()
-- interestedNow ic i = Ch.sendOnce ic (i, True)


-- current :: (MonadWidget t m) =>
--   Cache t m x ->
--   C t m (Dynamic t (Maybe x))
-- current cache = do
--   liftIO (atomically $ SMap.focus u item $ items cache) >>= \case
--     Nothing -> do
--       let changeE = leftmost . (locals cache :) . return . fmap head $
--             ffilter (not . null . filter ((== item) . relevant)) $ remotes cache
--       resultDyn <- holdDyn Nothing $ Just <$> changeE
--       liftIO . atomically $ SMap.insert (Right resultDyn) item $ items cache
--       return resultDyn
--     Just (Left Pending) -> liftIO . atomically $ do
--       SMap.lookup item (items cache) >>= \case
--         Just (Left Pending) -> do
--           safeIOToSTM $ putStrLn "Web.Channel.Client.Cache.ReadWrite.current: retrying."
--           retry
--         Just (Right d)      -> return d
--     Just (Right d)      -> return d
-- current :: (Hashable id, Eq id, MonadWidget t m) =>
--   Cache t m id a -> (a -> id) -> id ->
--   C t m (Dynamic t (Maybe a))
-- current cache relevant item = do
--   let u :: StrategyM STM
--         (Either Pending (Dynamic t (Maybe a)))
--         (Maybe (Either Pending (Dynamic t (Maybe a))))
--       u = \case
--         Nothing -> return (Nothing, Replace $ Left Pending)
--         r       -> return (r      , Keep)
--   liftIO (atomically $ SMap.focus u item $ items cache) >>= \case
--     Nothing -> do
--     -- TODO: design a better way to deal with multiple changes.
--       let changeE = leftmost . (locals cache :) . return . fmap head $
--             ffilter (not . null . filter ((== item) . relevant)) $ remotes cache
--       resultDyn <- holdDyn Nothing $ Just <$> changeE
--       liftIO . atomically $ SMap.insert (Right resultDyn) item $ items cache
--       return resultDyn
--     Just (Left Pending) -> liftIO . atomically $ do
--       SMap.lookup item (items cache) >>= \case
--         Just (Left Pending) -> do
--           safeIOToSTM $ putStrLn "Web.Channel.Client.Cache.ReadWrite.current: retrying."
--           retry
--         Just (Right d)      -> return d
--     Just (Right d)      -> return d

-- update :: Cache t m id a -> a -> C t m ()
-- update cache a = trigger cache a

