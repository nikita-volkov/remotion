module MessagingService.ConnectionT where

import MessagingService.Util.Prelude
import qualified Network.Socket
import qualified Pipes.ByteString as PipesByteString
import qualified Pipes.Prelude as PipesPrelude
import qualified System.Timeout as Timeout


-- | A connection, which can be used in both the server and the client.
newtype ConnectionT i o m r = 
  ConnectionT (ReaderT Settings (EitherT Failure m) r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Settings, MonadError Failure)

type Settings = (Handle, Timeout)

-- |
-- A connection timeout in microseconds. 
type Timeout = Int

data Failure =
  NoConnection |
  TimeoutReached |
  CorruptData Text
  deriving (Show)

instance MonadTrans (ConnectionT i o) where
  lift = ConnectionT . lift . lift

run :: ConnectionT i o m r -> Settings -> m (Either Failure r)
run (ConnectionT t) settings = runReaderT t settings |> runEitherT

ioeToFailure :: IOException -> Failure
ioeToFailure e = ioeGetErrorType e |> \case
  ResourceVanished -> NoConnection
  _ -> $bug $ "Unexpected IOError: " <> show e

receive :: (Serializable IO i, MonadIO m) => ConnectionT i o m i
receive = ConnectionT $ do
  (handle, timeout) <- ask
  let pipe = PipesByteString.fromHandle handle >-> deserializingPipe
  pipe |> PipesPrelude.head |> runEitherT |> Timeout.timeout timeout |> try |> liftIO >>= \case
    Right (Just (Right (Just r))) -> return r
    Right (Just (Right Nothing)) -> throwError $ CorruptData "No data"
    Right (Just (Left t)) -> throwError $ CorruptData t
    Right Nothing -> throwError $ TimeoutReached
    Left ioe -> throwError $ ioeToFailure ioe
  
send :: (Serializable IO o, MonadIO m, Applicative m) => o -> ConnectionT i o m ()
send a = ConnectionT $ do
  (handle, timeout) <- ask
  let pipe = serializingProducer a >-> PipesByteString.toHandle handle
  lift $ do
    tr <- fmapLT ioeToFailure $ tryIO $ Timeout.timeout timeout $ runEffect $ pipe
    failWith TimeoutReached tr

