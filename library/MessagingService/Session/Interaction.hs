module MessagingService.Session.Interaction where

import MessagingService.Util.Prelude hiding (State)
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified MessagingService.ConnectionT as C


-- Protocol
-----------------------------

data Request a = 
  Keepalive | 
  CloseSession |
  UserRequest a
  deriving (Generic)

instance (Serializable m a) => Serializable m (Request a)

type Response a = Either Failure (Maybe a)

data Failure = 
  CorruptRequest Text | 
  TimeoutReached
  deriving (Show, Generic)

instance Serializable m Failure


-- Client
-----------------------------

runClientSide :: 
  (MonadIO m, Applicative m, Serializable IO i, Serializable IO o) => 
  Request i -> C.ConnectionT m (Response o)
runClientSide req = do
  C.send req
  C.receive
  -- catchError C.receive $ \e -> do
  --   C.send CloseSession
  --   throwError e


-- Server
-----------------------------

-- | 
-- A function which processes messages from client (@i@) and produces a response (@o@),
-- while maintaining a user-defined session state (@s@) per each client.
-- 
-- This function essentially is what defines what your server actually does.
type ProcessUserRequest i o s = State s -> i -> IO o

-- |
-- A mutable state associated with particular client's connection.
-- Since we're in `IO` anyway, we use a mutable state with `IORef` wrapper.
-- You're free to extend it with whatever the data structure you want.
type State s = IORef (Maybe s)

-- runServerSide processUserRequest = do
--   req <- catchError C.receive $ \e -> do
--     case e of
--       C.TimeoutReached -> C.send $ Left $ TimeoutReached
--       C.CorruptData t -> C.send $ Left $ CorruptRequest t
--       _ -> return ()
--     throwError e
--   case req of
--     Keepalive -> C.send $ Right $ Nothing
--     CloseSession -> $notImplemented
--     UserRequest a -> $notImplemented

