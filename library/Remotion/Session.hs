module Remotion.Session where

import Remotion.Util.Prelude
import qualified Network.Socket
import qualified Pipes.ByteString as PipesByteString
import qualified Pipes.Prelude as PipesPrelude
import qualified System.Timeout as Timeout
import qualified Control.Exception as Ex


-- | 
-- An abstraction over networking and data transmission.
-- Can be used in implementation of both the server and the client.
newtype Session m r = 
  Session { unSession :: ReaderT Settings (EitherT Failure m) r }
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
  ReceiveTimeoutReached Int |
  SendTimeoutReached Int |
  CorruptData Text
  deriving (Show)


run :: Session m r -> Settings -> m (Either Failure r)
run (Session t) settings = runReaderT t settings |> runEitherT

adaptIOException :: IOException -> Failure
adaptIOException e = ioeGetErrorType e |> \case
  ResourceVanished -> ConnectionInterrupted
  _ -> $bug $ "Unexpected IOError: " <> (packText . show) e

adaptException :: SomeException -> Failure
adaptException e = if
  | Just ioe <- fromException e -> adaptIOException ioe
  | otherwise -> $bug $ "Unexpected exception: " <> packText (show e)

receive :: (Serializable IO i, MonadIO m) => Session m i
receive = Session $ do
  (handle, timeout) <- ask
  let pipe = PipesByteString.fromHandle handle >-> deserializingPipe
  pipe |> PipesPrelude.head |> runEitherT |> Ex.try |> Timeout.timeout timeout |> liftIO >>= \case
    Just (Right (Right (Just r))) -> return r
    Just (Right (Right Nothing)) -> throwError $ ConnectionInterrupted
    Just (Right (Left t)) -> throwError $ CorruptData t
    Just (Left e) -> throwError $ adaptIOException e
    Nothing -> throwError $ ReceiveTimeoutReached timeout
  
send :: (Serializable IO o, MonadIO m, Applicative m) => o -> Session m ()
send a = Session $ do
  (handle, timeout) <- ask
  let pipe = serializingProducer a >-> PipesByteString.toHandle handle
  lift $ do
    tr <- fmapLT adaptIOException $ tryIO $ Timeout.timeout timeout $ runEffect $ pipe
    failWith (SendTimeoutReached timeout) tr


instance MonadTrans Session where
  lift = Session . lift . lift

instance (MonadBase IO m) => MonadBase IO (Session m) where
  liftBase = Session . liftBase

instance MonadTransControl Session where
  newtype StT Session a = StT { unStT :: Either Failure a }
  liftWith runToBase = do
    settings <- Session $ ask
    Session $ lift $ lift $ runToBase $ liftM StT . flip run settings
  restoreT base = do
    StT r <- Session $ lift $ lift $ base
    Session $ lift $ hoistEither r

instance (MonadBaseControl IO m) => MonadBaseControl IO (Session m) where
  newtype StM (Session m) a = StMT { unStMT :: ComposeSt Session m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM = defaultRestoreM unStMT
