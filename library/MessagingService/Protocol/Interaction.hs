module MessagingService.Protocol.Interaction where

import MessagingService.Util.Prelude


-- Interaction
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

