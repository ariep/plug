{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Web.Channel.Client.Cache where

import Common

import           Control.Concurrent.STM    (STM, atomically, retry)
import           Control.Coroutine.Monadic (Repeat, Eps, (:!:), (:?:))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Hashable             (Hashable)
import           Data.SafeCopy             (SafeCopy)
import           Focus                     (StrategyM, Decision(Keep, Replace))
import           Reflex                    (Dynamic, Event, holdDyn)
import           Reflex.Dom                (MonadWidget)
import qualified STMContainers.Map as SMap
import           Web.Channel               (Channel)
import           Web.Channel.Client        (sendOnceReceiptTag)
import           Web.Widgets               (C, Make, Address, Tag)


type Cache t query result
  = SMap.Map query (Either Pending (Dynamic t (Maybe result)))

createEmpty :: (MonadIO m) => C t m (Cache t query result)
createEmpty = liftIO SMap.newIO

data Pending
  = Pending

data Query t m query result
  = Query
    { queryID :: query
    , request :: C t m (Event t result)
    }

lookupOnce :: (MonadWidget t m, SafeCopy a, SafeCopy b) =>
  Channel (Repeat (a :?: b :!: Eps)) -> (a -> Tag) -> a ->
  Query t m a b
lookupOnce ch tag a = Query a
  (sendOnceReceiptTag ch (tag a) a)

-- lookupLive :: Channel (Repeat (a :?: b :!: Eps)) -> (a -> Tag) -> a ->
--   Query a b
-- lookupLive ch tag a = Query a
--   (sendManyReceiptTag ch (tag a) a)

lookup :: (Hashable query, Eq query, MonadWidget t m) =>
  Cache t query result -> Query t m query result ->
  C t m (Dynamic t (Maybe result))
lookup c q = do
  let u :: StrategyM STM
        (Either Pending (Dynamic t (Maybe result)))
        (Maybe (Either Pending (Dynamic t (Maybe result))))
      u = \case
        Nothing -> return (Nothing, Replace $ Left Pending)
        r       -> return (r      , Keep)
  liftIO (atomically $ SMap.focus u (queryID q) c) >>= \case
    Nothing -> do
      resultE <- request q
      resultDyn <- holdDyn Nothing $ Just <$> resultE
      liftIO . atomically $ SMap.insert (Right resultDyn) (queryID q) c
      return resultDyn
    Just (Left Pending) -> liftIO . atomically $ do
      SMap.lookup (queryID q) c >>= \case
        Just (Left Pending) -> do
          safeIOToSTM $ putStrLn "Web.Channel.Client.Cache.lookup: retrying."
          retry
        Just (Right d)      -> return d
    Just (Right d)      -> liftIO (putStrLn "Reusing cached result") >> return d

-- refresh :: (Hashable query, Eq query, MonadWidget t m) =>
--   Cache t query result -> Query t m query result ->
--   C t m ()
-- refresh cache query = 

