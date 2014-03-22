module MessagingService.Server.Session where

import MessagingService.Util.Prelude hiding (State, listen, interact)
import qualified MessagingService.Protocol.Request as Rq
import qualified MessagingService.Protocol.Response as Rs
import qualified MessagingService.Util.FileSystem as FS
import qualified Network
import qualified Network.Socket
import qualified Pipes.Network.TCP.Safe as PipesNetwork
import qualified Pipes.Prelude as PipesPrelude


-- | 
-- A user session on server.
newtype Session i o s r = 
  Session (ReaderT (Env i o s) (EitherT Failure IO) r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Env i o s), MonadError Failure)
type Env i o s = (Settings i o s, SessionState s)
type Settings i o s = (Network.Socket.Socket, Timeout, Authenticate, ProcessMessage i o s)

-- |
-- A session timeout in microseconds. 
-- The period of keepalive signaling depends on that parameter.
-- If you don't want excessive requests, just make it a couple of minutes.
type Timeout = Int

-- | 
-- A function, which checks the hashed authentication data.
-- If you want to provide access to anybody, use @(\_ -> return True)@.
-- 
-- An argument value of @Nothing@ means an attempt of anonymous authentication.
type Authenticate = Maybe Credentials -> IO Bool

-- |
-- Either a plain ASCII password or an encoding of some data, 
-- e.g. an MD5 hash of a login-password pair or just a password.
type Credentials = ByteString

-- | 
-- A function which processes messages from client (@i@) and produces a response (@o@),
-- while maintaining a user-defined session state (@s@) per each client.
-- 
-- This function essentially is what defines what your server actually does.
type ProcessMessage i o s = State s -> i -> IO o

-- |
-- A mutable state associated with particular client's connection.
-- Since we're in `IO` anyway, we use a mutable state with `IORef` wrapper.
-- You're free to extend it with whatever the data structure you want.
type State s = IORef (Maybe s)

type SessionState s = (Authenticated, State s)

type Authenticated = IORef Bool

data Failure =
  NoConnection |
  TimeoutReached |
  CorruptRequest Text |
  EmptyRequest
  deriving (Show)

run :: Session i o s r -> Settings i o s -> IO (Either Failure r)
run (Session t) settings = do
  state <- liftIO $ (,) <$> newIORef False <*> newIORef Nothing
  runReaderT t (settings, state) |> runEitherT

ioeToFailure :: IOException -> Failure
ioeToFailure e = ioeGetErrorType e |> \case
  TimeExpired -> TimeoutReached
  ResourceVanished -> NoConnection
  _ -> $bug $ "Unexpected IOError: " <> show e

receive :: (Serializable IO i) => Session i o s (Rq.Request i)
receive = Session $ do
  pipe <- do
    ((socket, timeout, _, _), _) <- ask
    return $ 
      PipesNetwork.fromSocketTimeout timeout socket 4096 >-> deserializingPipe
  pipe |> PipesPrelude.head |> runEitherT |> try |> liftIO >>= \case
    Right (Right (Just r)) -> return r
    Right (Right Nothing) -> throwError $ EmptyRequest
    Right (Left t) -> throwError $ CorruptRequest t
    Left ioe -> throwError $ ioeToFailure ioe
  
send :: (Serializable IO o) => Rs.Response o -> Session i o s ()
send a = Session $ do
  pipe <- do 
    ((socket, timeout, _, _), _) <- ask
    return $ 
      serializingProducer a >-> PipesNetwork.toSocketTimeout timeout socket
  lift $ fmapLT ioeToFailure $ tryIO $ runEffect $ pipe

interact :: (Serializable IO i, Serializable IO o) => Session i o s ()
interact = do
  ((_, timeout, auth, processMessage), (authenticated, state)) <- ask
  let   
    checkingAuthentication session = do
      readIORef authenticated |> liftIO >>= \case
        True -> session
        False -> send $ Left Rs.Unauthenticated
    receive' = catchError receive $ \e -> do
      case e of
        EmptyRequest -> send $ Left $ Rs.CorruptRequest "No data"
        TimeoutReached -> send $ Left $ Rs.TimeoutReached
        CorruptRequest t -> send $ Left $ Rs.CorruptRequest t
        _ -> return ()
      throwError e
  receive' >>= \case
    Left cmd -> case cmd of
      Rq.Authenticate hash -> do
        liftIO (auth hash) >>= \case
          True -> do
            send $ Right (Nothing, timeout)
            liftIO $ writeIORef authenticated True
            interact
          False -> do
            send $ Left Rs.Unauthenticated
      Rq.CloseSession -> do
        send $ Right (Nothing, timeout)
      Rq.Ping -> do
        send $ Right (Nothing, timeout)
        interact
    Right m -> do
      checkingAuthentication $ do
        reply <- liftIO $ processMessage state m
        send $ Right (Just reply, timeout)
        interact

sendTooManyConnections :: (Serializable IO o) => Session i o s ()
sendTooManyConnections = send $ Left $ Rs.TooManyConnections

sendOkay :: (Serializable IO o) => Session i o s ()
sendOkay = do
  ((_, timeout, _, _), _) <- ask
  send $ Right $ (Nothing, timeout)

