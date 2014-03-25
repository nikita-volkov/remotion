module MessagingService.Server.Session where

import MessagingService.Util.Prelude hiding (State, listen, interact)
import qualified MessagingService.ConnectionT as C
import qualified MessagingService.Protocol.Client as PC
import qualified MessagingService.Protocol.Server as PS


-- | 
-- A user session on server.
newtype Session i o s r = 
  Session (ReaderT (Env i o s) (C.ConnectionT (PC.Message i) (PS.Message o) IO) r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Env i o s), MonadError C.Failure)
type Env i o s = (Settings i o s, SessionState s)
type Settings i o s = (Timeout, Authenticate, ProcessMessage i o s)

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

-- |
-- A session timeout in microseconds. 
-- The period of keepalive signaling depends on that parameter.
-- If you don't want excessive requests, just make it a couple of minutes.
type Timeout = Int

run :: Session i o s r -> (C.Settings, Settings i o s) -> IO (Either C.Failure r)
run (Session t) (connectionSettings, settings) = do
  state <- liftIO $ (,) <$> newIORef False <*> newIORef Nothing
  runReaderT t (settings, state) |> flip C.run connectionSettings

receive :: (Serializable IO i, Serializable IO o) => Session i o s (PC.Message i)
receive = do
  Session $ lift $ catchError C.receive $ \e -> do
    case e of
      C.TimeoutReached -> C.send $ PS.TimeoutReached
      C.CorruptData t -> C.send $ PS.CorruptRequest t
      _ -> return ()
    throwError e

send :: (Serializable IO o) => PS.Message o -> Session i o s ()
send response = Session $ lift $ C.send response

interact :: (Serializable IO i, Serializable IO o) => Session i o s ()
interact = do
  ((timeout, auth, processMessage), (authenticated, state)) <- Session $ ask
  let   
    checkingAuthentication session = do
      readIORef authenticated |> liftIO >>= \case
        True -> session
        False -> send $ PS.Unauthenticated
  receive >>= \case
    PC.Authenticate hash -> do
      liftIO (auth hash) >>= \case
        True -> do
          send $ PS.Okay Nothing
          liftIO $ writeIORef authenticated True
          interact
        False -> do
          send $ PS.Unauthenticated
    PC.CloseSession -> do
      send $ PS.Okay Nothing
    PC.Ping -> do
      checkingAuthentication $ do
        send $ PS.Okay Nothing
        interact
    PC.Data m -> do
      checkingAuthentication $ do
        reply <- liftIO $ processMessage state m
        send $ PS.Okay $ Just reply
        interact

rejectWithTooManyConnections :: (Serializable IO o) => Session i o s ()
rejectWithTooManyConnections = send $ PS.TooManyConnections

greet :: (Serializable IO o) => Session i o s ()
greet = do
  ((timeout, _, _), _) <- ask
  send $ PS.Welcome timeout

standard :: (Serializable IO i, Serializable IO o) => Session i o s ()
standard = greet >> interact
