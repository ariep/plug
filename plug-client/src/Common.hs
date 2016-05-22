module Common where

import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar   (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM    (STM)
import           Control.Exception         (SomeException, throw, catch)
import           GHC.Conc.Sync             (unsafeIOToSTM)

safeIOToSTM :: IO a -> STM a
safeIOToSTM req = unsafeIOToSTM $ do
  tv <- newEmptyMVar
  forkIO $
    (req >>= putMVar tv . Right)
      `Control.Exception.catch`
      (\ (e :: SomeException) -> putMVar tv $ Left e)
  r <- takeMVar tv
  case r of
    Right x -> return x
    Left e  -> throw e
