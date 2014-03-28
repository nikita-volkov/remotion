module MessagingService.Server.Session where

import MessagingService.Util.Prelude hiding (State, listen, interact)
import MessagingService.Protocol.Interaction 
import qualified MessagingService.SessionT as SessionT; import MessagingService.SessionT (SessionT)



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
  ProcessUserRequest i o s -> SessionT m ()
interact processRequest = do
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

