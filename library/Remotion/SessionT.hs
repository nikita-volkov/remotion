module Remotion.SessionT where

import Remotion.Util.Prelude
import qualified Network.Socket
import qualified Pipes.ByteString as PipesByteString
import qualified Pipes.Prelude as PipesPrelude
import qualified System.Timeout as Timeout
import qualified Control.Exception as Ex


-- | 
-- An abstraction over networking and data transmission.
-- Can be used in implementation of both the server and the client.
newtype SessionT m r = 
  SessionT { unSessionT :: ReaderT Settings (EitherT Failure m) r }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Settings, MonadError Failure)

type Settings = (Socket, Timeout)

type Socket = Handle

-- |
-- A connection timeout in microseconds.
-- The period of keepalive signaling depends on that parameter.
-- If you don't want excessive requests, just make it a couple of minutes.
type Timeout = Int

data Failure =
  ConnectionInterrupted |
  TimeoutReached |
  CorruptData Text
  deriving (Show)


run :: SessionT m r -> Settings -> m (Either Failure r)
run (SessionT t) settings = runReaderT t settings |> runEitherT

ioeToFailure :: IOException -> Failure
ioeToFailure e = ioeGetErrorType e |> \case
  ResourceVanished -> ConnectionInterrupted
  _ -> $bug $ "Unexpected IOError: " <> (packText . show) e

receive :: (Serializable IO i, MonadIO m) => SessionT m i
receive = SessionT $ do
  (handle, timeout) <- ask
  let pipe = PipesByteString.fromHandle handle >-> deserializingPipe
  pipe |> PipesPrelude.head |> runEitherT |> Timeout.timeout timeout |> Ex.try |> liftIO >>= \case
    Right (Just (Right (Just r))) -> return r
    Right (Just (Right Nothing)) -> throwError $ CorruptData "No data"
    Right (Just (Left t)) -> throwError $ CorruptData t
    Right Nothing -> throwError $ TimeoutReached
    Left ioe -> throwError $ ioeToFailure ioe
  
send :: (Serializable IO o, MonadIO m, Applicative m) => o -> SessionT m ()
send a = SessionT $ do
  (handle, timeout) <- ask
  let pipe = serializingProducer a >-> PipesByteString.toHandle handle
  lift $ do
    tr <- fmapLT ioeToFailure $ tryIO $ Timeout.timeout timeout $ runEffect $ pipe
    failWith TimeoutReached tr


instance MonadTrans SessionT where
  lift = SessionT . lift . lift

instance (MonadBase IO m) => MonadBase IO (SessionT m) where
  liftBase = SessionT . liftBase

instance MonadTransControl SessionT where
  newtype StT SessionT a = StT { unStT :: Either Failure a }
  liftWith runToBase = do
    settings <- SessionT $ ask
    SessionT $ lift $ lift $ runToBase $ liftM StT . flip run settings
  restoreT base = do
    StT r <- SessionT $ lift $ lift $ base
    SessionT $ lift $ hoistEither r

instance (MonadBaseControl IO m) => MonadBaseControl IO (SessionT m) where
  newtype StM (SessionT m) a = StMT { unStMT :: ComposeSt SessionT m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM = defaultRestoreM unStMT
