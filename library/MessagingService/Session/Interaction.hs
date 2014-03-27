module MessagingService.Session.Interaction where

import MessagingService.Util.Prelude hiding (State)
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified MessagingService.Session as S


-- Protocol
-----------------------------

data Request a = 
  Keepalive | 
  CloseSession |
  UserRequest a
  deriving (Generic)

instance (Serializable m a) => Serializable m (Request a)

type Response a = Either Failure (Maybe a)

-- |
-- A failure response from server.
data Failure = 
  -- | 
  -- Server was unable to deserialize the request.
  -- This is only expected happen in case of user's protocol mismatch.
  CorruptRequest Text | 
  -- |
  -- A connection keepalive timeout reached.
  TimeoutReached
  deriving (Show, Generic)

instance Serializable m Failure

