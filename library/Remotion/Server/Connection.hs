module Remotion.Server.Connection where

import Remotion.Util.Prelude hiding (State, listen, interact)
import qualified Remotion.Protocol as P
import qualified Remotion.Session as S


runConnection :: 
  (MonadIO m, Applicative m, Serializable IO i, Serializable IO o) =>
  S.Socket ->
  ServerIsAvailable ->
  Authenticate ->
  P.Timeout ->
  P.UserProtocolSignature ->
  ProcessUserRequest i o s -> 
  m (Either ConnectionFailure ())
runConnection socket available authenticate timeout userVersion processRequest = runEitherT $ do
  do 
    r <- lift $ S.run (handshake available authenticate timeout userVersion) (socket, 10^6*3) 
    hoistEither $ join . liftM (fmapL HandshakeFailure) $ fmapL SessionFailure r
  do
    r <- lift $ S.run (interact processRequest) (socket, timeout)
    hoistEither $ fmapL SessionFailure r

data ConnectionFailure = 
  HandshakeFailure P.HandshakeFailure |
  SessionFailure S.Failure
  deriving (Show)


-- Handshake
-----------------------------

-- | 
-- A function, which checks the authentication data.
-- If you want to provide access to anybody, use @(const $ return True)@.
type Authenticate = P.Credentials -> IO Bool

-- |
-- 
type ServerIsAvailable = Bool

handshake ::
  (MonadIO m, Applicative m) =>
  ServerIsAvailable ->
  Authenticate ->
  P.Timeout ->
  P.UserProtocolSignature ->
  S.Session m (Either P.HandshakeFailure ())
handshake available authenticate timeout userVersion = runEitherT $ do
  do
    check (not available) $ P.ServerIsBusy
  do
    cv <- receive
    check (cv /= P.version) $ P.ProtocolVersionMismatch cv P.version
  do
    cv <- receive
    check (cv /= userVersion) $ P.UserProtocolSignatureMismatch cv userVersion
  do
    credentials <- receive
    ok <- liftIO $ authenticate $ credentials
    check (not ok) $ P.Unauthenticated
  do
    0::Int <- receive -- A workaround for otherwise unpredictable behaviour,
                      -- happening in case of multiple sends.
    send $ timeout
  where
    receive = lift $ S.receive
    send = lift . S.send
    check condition failure = do
      let failureM = if condition then Just $ failure else Nothing
      send failureM
      maybe (return ()) left failureM


-- Interaction
-----------------------------

-- | 
-- A function which processes requests of type @i@ from client and 
-- produces a response of type @o@,
-- while maintaining a user-defined session state of type @s@ per each client.
-- 
-- This function essentially is what defines what the server actually does.
type ProcessUserRequest i o s = State s -> i -> IO o

-- |
-- A mutable state associated with particular client's connection.
-- Since we're in `IO` anyway, we use a mutable state with `IORef` wrapper.
-- You're free to extend it with whatever the data structure you want.
type State s = IORef (Maybe s)

interact :: 
  forall i o s m. 
  (MonadIO m, Serializable IO i, Serializable IO o, Applicative m) =>
  ProcessUserRequest i o s -> S.Session m ()
interact processRequest = do
  state <- liftIO $ newIORef Nothing
  let 
    loop = do
      i <- catchError receive $ \e -> do
        case e of
          S.ReceiveTimeoutReached t -> send $ Left $ P.TimeoutReached t
          S.SendTimeoutReached t -> send $ Left $ P.TimeoutReached t
          S.CorruptData t -> send $ Left $ P.CorruptRequest t
          _ -> return ()
        throwError e
      case i of
        P.CloseSession -> do
          send $ Right $ Nothing
        P.Keepalive -> do
          send $ Right $ Nothing
          loop
        P.UserRequest a -> do
          o <- liftIO $ processRequest state a
          send $ Right $ Just o
          loop
  loop
  where
    receive = S.receive :: S.Session m (P.Request i)
    send = S.send :: P.Response o -> S.Session m ()

