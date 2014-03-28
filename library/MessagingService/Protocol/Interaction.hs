module MessagingService.Protocol.Interaction where

import MessagingService.Util.Prelude hiding (State)
import qualified MessagingService.SessionT as SessionT; import MessagingService.SessionT (SessionT)


-- Protocol
-----------------------------

data Request a = 
  Keepalive | 
  CloseSession |
  UserRequest a
  deriving (Generic)

instance (Serializable m a) => Serializable m (Request a)

type Response a = Either FailureResponse (Maybe a)

-- |
-- A failure response from server.
data FailureResponse = 
  -- | 
  -- Server was unable to deserialize the request.
  -- This is only expected happen in case of user's protocol mismatch.
  CorruptRequest Text | 
  -- |
  -- A connection keepalive timeout reached.
  TimeoutReached
  deriving (Show, Generic)

instance Serializable m FailureResponse


-- Server-side
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

runServerSide :: 
  forall i o s m. 
  (MonadIO m, Serializable IO i, Serializable IO o, Applicative m) =>
  ProcessUserRequest i o s -> SessionT m ()
runServerSide processRequest = do
  state <- liftIO $ newIORef Nothing
  let 
    loop = do
      i <- catchError receive $ \e -> do
        case e of
          SessionT.TimeoutReached -> send $ Left $ TimeoutReached
          SessionT.CorruptData t -> send $ Left $ CorruptRequest t
          _ -> return ()
        throwError e
      case i of
        CloseSession -> do
          send $ Right $ Nothing
        Keepalive -> do
          send $ Right $ Nothing
          loop
        UserRequest a -> do
          o <- liftIO $ processRequest state a
          send $ Right $ Just o
          loop
  loop
  where
    receive = SessionT.receive :: SessionT m (Request i)
    send = SessionT.send :: Response o -> SessionT m ()


-- Client-side
-----------------------------

-- runClientSide :: Request i -> SessionT m (Response o)
-- runClientSide
