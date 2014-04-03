module Remotion.Client.InteractionT where

import Remotion.Util.Prelude hiding (State, listen, interact)
import qualified Remotion.SessionT as S
import qualified Control.Concurrent.Lock as Lock


-- | Provides support for safe concurrenct interaction.
newtype InteractionT i o m r = 
  InteractionT { unInteractionT :: ReaderT Lock (S.SessionT m) r }
  deriving (Functor, Applicative, Monad, MonadError S.Failure, MonadIO)

-- | Ensures a response to request accomodation in concurrency.
type Lock = Lock.Lock

run :: (MonadIO m) => InteractionT i o m r -> S.Settings -> m (Either S.Failure r)
run m settings = do
  lock <- liftIO $ Lock.new
  m |> unInteractionT |> flip runReaderT lock |> flip S.run settings

interact :: 
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m) => 
  i -> InteractionT i o m o
interact = \request -> withLock $ send request *> receive 
  where
    withLock action = do
      l <- InteractionT ask
      lock l *> action <* unlock l
      where
        lock = InteractionT . liftIO . Lock.acquire
        unlock = InteractionT . liftIO . Lock.release
    send r = InteractionT $ lift $ S.send r
    receive = InteractionT $ lift $ S.receive


instance MonadTrans (InteractionT i o) where
  lift = InteractionT . lift . lift

instance (MonadBase IO m) => MonadBase IO (InteractionT i o m) where
  liftBase = InteractionT . liftBase

instance MonadTransControl (InteractionT i o) where
  newtype StT (InteractionT i o) a = StT (StT S.SessionT a)
  liftWith runInM = do
    env <- InteractionT $ ask
    InteractionT $ lift $ liftWith $ \runSessionT -> runInM $ 
      liftM StT . runSessionT . flip runReaderT env . unInteractionT
  restoreT m = do
    InteractionT $ lift $ do
      StT r <- lift $ m
      restoreT $ return $ r

instance (MonadBaseControl IO m) => MonadBaseControl IO (InteractionT i o m) where
  newtype StM (InteractionT i o m) a = StMT { unStMT :: ComposeSt (InteractionT i o) m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM = defaultRestoreM unStMT
