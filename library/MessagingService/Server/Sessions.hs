module MessagingService.Server.Sessions where

import MessagingService.Util.Prelude hiding (State, listen, interact)
import qualified MessagingService.Protocol as P
import qualified MessagingService.SessionT as S


-- Handshake
-----------------------------

-- | 
-- A function, which checks the hashed authentication data.
-- If you want to provide access to anybody, use @(\_ -> return True)@.
type Authenticate = P.Credentials -> IO Bool

-- |
-- 
type ServerIsAvailable = Bool

handshake ::
  (MonadIO m, Applicative m) =>
  ServerIsAvailable ->
  Authenticate ->
  P.Timeout ->
  P.UserProtocolVersion ->
  S.SessionT m (Either P.HandshakeFailure ())
handshake available authenticate timeout userVersion = runEitherT $ do
  do
    check (not available) $ P.ServerIsBusy
  do
    cv <- receive
    check (cv /= P.version) $ P.ProtocolVersionMismatch cv P.version
  do
    cv <- receive
    check (cv /= userVersion) $ P.UserProtocolVersionMismatch cv userVersion
  do
    credentials <- receive
    ok <- liftIO $ authenticate $ credentials
    check (not ok) $ P.Unauthenticated
  do
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
  ProcessUserRequest i o s -> S.SessionT m ()
interact processRequest = do
  state <- liftIO $ newIORef Nothing
  let 
    loop = do
      i <- catchError receive $ \e -> do
        case e of
          S.TimeoutReached -> send $ Left $ P.TimeoutReached
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
    receive = S.receive :: S.SessionT m (P.Request i)
    send = S.send :: P.Response o -> S.SessionT m ()

