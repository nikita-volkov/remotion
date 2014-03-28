module MessagingService.Server.Session where

import MessagingService.Util.Prelude hiding (State, listen, interact)
import qualified MessagingService.SessionT as SesisonT
import qualified MessagingService.Protocol.Interaction as Interaction


-- | 
-- A user session on server.
newtype Session i o s r = 
  Session (ReaderT (ProcessMessageWithState i o) (SesisonT.SessionT IO) r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError SesisonT.Failure)

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

type ProcessMessageWithState i o = i -> IO o

run :: 
  (SesisonT.Settings, ProcessMessage i o s) ->
  Session i o s r -> IO (Either SesisonT.Failure r)
run (connectionSettings, processMessage) (Session t) = do
  state <- liftIO $ newIORef Nothing
  runReaderT t (processMessage state) |> flip SesisonT.run connectionSettings

receive :: (Serializable IO i, Serializable IO o) => Session i o s (Interaction.Request i)
receive = do
  catchError (Session $ lift SesisonT.receive) $ \e -> do
    case e of
      SesisonT.TimeoutReached -> send $ Left $ Interaction.TimeoutReached
      SesisonT.CorruptData t -> send $ Left $ Interaction.CorruptRequest t
      _ -> return ()
    throwError e

send :: (Serializable IO o) => Interaction.Response o -> Session i o s ()
send response = Session $ lift $ SesisonT.send response

interact :: (Serializable IO i, Serializable IO o) => Session i o s ()
interact = do
  processMessage <- Session $ ask
  receive >>= \case
    Interaction.CloseSession -> do
      send $ Right $ Nothing
    Interaction.Keepalive -> do
      send $ Right $ Nothing
      interact
    Interaction.UserRequest m -> do
      reply <- liftIO $ processMessage m
      send $ Right $ Just reply
      interact
