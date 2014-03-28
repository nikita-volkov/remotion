{-# LANGUAGE MagicHash, UnboxedTuples #-}
module MessagingService.Util.Forking 
  (
    forkRethrowingFinally,
    forkRethrowingFinallyWithWait,
    forkRethrowingWithWait,
    myThreadId,
    killThread,
  )
  where

import MessagingService.Util.Prelude
import Control.Concurrent
import qualified GHC.IO as GHC
import qualified GHC.Conc as GHC


-- |
-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE forkIO' #-}
forkIO' :: IO () -> IO ThreadId
forkIO' action = GHC.IO $ \s -> case (fork# action s) of (# s1, tid #) -> (# s1, GHC.ThreadId tid #)

forkRethrowingFinally :: IO () -> IO () -> IO ThreadId
forkRethrowingFinally finally io = do
  parentTID <- myThreadId
  childTID <- mask $ \restore -> forkIO' $ do
    let rethrowingFinally = catch finally $ \(SomeException e) -> throwTo parentTID e
    try (restore io) >>= \case
      Left e | Just ThreadKilled <- fromException e -> rethrowingFinally
      Left (SomeException e) -> rethrowingFinally >> throwTo parentTID e
      Right r -> rethrowingFinally
  return childTID

forkRethrowingFinallyWithWait :: IO () -> IO a -> IO (ThreadId, IO (Maybe a))
forkRethrowingFinallyWithWait finally io = do
  var <- newEmptyMVar
  parentTID <- myThreadId
  childTID <- mask $ \restore -> forkIO' $ do
    let rethrowingFinally = catch finally $ \(SomeException e) -> throwTo parentTID e
    try (restore io) >>= \case
      Left e | Just ThreadKilled <- fromException e -> rethrowingFinally >> putMVar var Nothing
      Left (SomeException e) -> rethrowingFinally >> throwTo parentTID e >> putMVar var Nothing
      Right r -> rethrowingFinally >> putMVar var (Just r)
  return (childTID, takeMVar var)

forkRethrowingWithWait :: IO a -> IO (ThreadId, IO (Maybe a))
forkRethrowingWithWait = forkRethrowingFinallyWithWait (return ())
