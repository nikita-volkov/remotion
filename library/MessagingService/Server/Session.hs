module MessagingService.Server.Session where

import MessagingService.Util.Prelude hiding (State, listen, interact)
import qualified MessagingService.Protocol as Protocol
import qualified MessagingService.Util.FileSystem as FS
import qualified Network
import qualified Network.Socket
import qualified Pipes.Network.TCP.Safe as PipesNetwork
import qualified Pipes.Prelude as PipesPrelude


-- | 
-- A user session on server.
newtype Session u a r = 
  Session (EitherT Text (ReaderT (Env u) IO) r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Env u), MonadError Text)
type Env u = (Settings, State u)
type Settings = (Network.Socket.Socket, Timeout, Authenticate)
type Timeout = Int
type Authenticate = Maybe Hash -> IO Bool
type Hash = ByteString
type State u = (Authenticated, u)
type Authenticated = IORef Bool

run :: Session u a r -> Env u -> IO (Either Text r)
run (Session t) env = runReaderT (runEitherT t) env

listen :: (Serializable (ReaderT (Env u) IO) a) => Session u a (Protocol.Request a)
listen = Session $ do
  ((socket, timeout, _), _) <- ask
  failWith "Empty request" =<< do 
    PipesPrelude.head $ PipesNetwork.fromSocketTimeout timeout socket 4096 >-> deserializingPipe
  
reply :: (Serializable (Session u a) a) => Protocol.Response a -> Session u a ()
reply a = do
  ((socket, timeout, _), _) <- ask
  runEffect $ serializingProducer a >-> PipesNetwork.toSocketTimeout timeout socket

interact :: (Serializable (ReaderT (Env u) IO) a, Serializable (Session u a) a) => Session u a ()
interact = do
  listen >>= \case
    Protocol.Request_StartSession hash -> do
      ((_, _, auth), _) <- ask
      liftIO (auth hash) >>= \case
        True -> do
          reply $ Protocol.Response_StartSession True
          interact
        False -> do
          reply $ Protocol.Response_StartSession False
    Protocol.Request_CloseSession -> do
      reply $ Protocol.Response_CloseSession
    _ -> $notImplemented


