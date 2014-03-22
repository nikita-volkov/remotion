module MessagingService.ConnectionT where

import MessagingService.Util.Prelude
import qualified Network.Socket
import qualified Pipes.Network.TCP.Safe as PipesNetwork
import qualified Pipes.Prelude as PipesPrelude


-- | A connection, which can be used in both the server and the client.
newtype ConnectionT i o m r = 
  ConnectionT (ReaderT Settings (EitherT Failure m) r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Settings, MonadError Failure)

type Settings = (Network.Socket.Socket, Timeout)

-- |
-- A connection timeout in microseconds. 
type Timeout = Int

data Failure =
  NoConnection |
  TimeoutReached |
  CorruptData Text |
  EmptyRequest
  deriving (Show)

instance MonadTrans (ConnectionT i o) where
  lift = ConnectionT . lift . lift

run :: ConnectionT i o m r -> Settings -> m (Either Failure r)
run (ConnectionT t) settings = runReaderT t settings |> runEitherT

ioeToFailure :: IOException -> Failure
ioeToFailure e = ioeGetErrorType e |> \case
  TimeExpired -> TimeoutReached
  ResourceVanished -> NoConnection
  _ -> $bug $ "Unexpected IOError: " <> show e

receive :: (Serializable IO i, MonadIO m) => ConnectionT i o m i
receive = ConnectionT $ do
  pipe <- do
    (socket, timeout) <- ask
    return $ 
      PipesNetwork.fromSocketTimeout timeout socket 4096 >-> deserializingPipe
  pipe |> PipesPrelude.head |> runEitherT |> try |> liftIO >>= \case
    Right (Right (Just r)) -> return r
    Right (Right Nothing) -> throwError $ EmptyRequest
    Right (Left t) -> throwError $ CorruptData t
    Left ioe -> throwError $ ioeToFailure ioe
  
send :: (Serializable IO o, MonadIO m) => o -> ConnectionT i o m ()
send a = ConnectionT $ do
  pipe <- do 
    (socket, timeout) <- ask
    return $ 
      serializingProducer a >-> PipesNetwork.toSocketTimeout timeout socket
  lift $ fmapLT ioeToFailure $ tryIO $ runEffect $ pipe

