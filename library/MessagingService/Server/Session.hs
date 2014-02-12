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
newtype Session i o s r = 
  Session (ReaderT (Env i o s) (EitherT Text IO) r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Env i o s), MonadError Text)
type Env i o s = (Settings i o s, State s)
type Settings i o s = (Network.Socket.Socket, Timeout, Authenticate, ProcessMessage i o s)

-- |
-- A session timeout in ms. Period of keepalive signaling depends on that parameter.
-- If you don't want excessive requests, just make it a couple of minutes.
type Timeout = Int

-- | 
-- A function, which checks the hashed authentication data.
-- If you want to provide access to anybody, use @(\_ -> return True)@.
-- 
-- An argument value of @Nothing@ means an attempt of anonymous authentication.
type Authenticate = Maybe Hash -> IO Bool

-- |
-- Either a plain ASCII password or an encoding of some data, 
-- e.g. an MD5 hash of a login-password pair or just a password.
type Hash = ByteString

-- | 
-- A function which processes messages from client and produces a response,
-- while managing a user-defined session state for each client.
-- Since we're in `IO` to have a mutable state you can use mutable data structures and `IORef`s.
-- 
-- This function essentially is what defines what your server actually does.
type ProcessMessage i o s = s -> i -> IO o

type State s = (Authenticated, s)

type Authenticated = IORef Bool


run :: Session i o s r -> Env i o s -> EitherT Text IO r
run (Session t) = runReaderT t

listen :: (Serializable IO i) => Session i o s (Protocol.Request i)
listen = Session $ do
  ((socket, timeout, _, _), _) <- ask
  lift $ do
    failWith "Empty request" =<< do 
      PipesPrelude.head $ PipesNetwork.fromSocketTimeout timeout socket 4096 >-> deserializingPipe
  
reply :: (Serializable IO o) => Protocol.Response o -> Session i o s ()
reply a = Session $ do
  ((socket, timeout, _, _), _) <- ask
  liftIO $ runEffect $ serializingProducer a >-> PipesNetwork.toSocketTimeout timeout socket

interact :: (Serializable IO i, Serializable IO o) => Session i o s ()
interact = do
  ((_, _, auth, processMessage), (authenticated, s)) <- ask
  listen >>= \case
    Protocol.Request_StartSession hash -> do
      liftIO (auth hash) >>= \case
        True -> do
          reply $ Protocol.Response_StartSession True
          interact
        False -> do
          reply $ Protocol.Response_StartSession False
    Protocol.Request_CloseSession -> do
      reply $ Protocol.Response_CloseSession
    _ -> $notImplemented


