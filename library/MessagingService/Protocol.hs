module MessagingService.Protocol where

import MessagingService.Util.Prelude


-- Handshake
-----------------------------

-- |
-- A version of the internal protocol used for checking of server-client match.
type ProtocolVersion = Int

-- |
-- A user-supplied version of user's protocol
-- used for checking of server-client match.
type UserProtocolVersion = Int

-- |
-- Either a plain ASCII password or an encoding of some data, 
-- e.g. an MD5 hash of a login-password pair or just a password.
-- 
-- @Nothing@ means anonymous.
type Credentials = Maybe ByteString

-- |
-- A session timeout in microseconds. 
-- The period of keepalive signaling depends on that parameter.
-- If you don't want excessive requests, just make it a couple of minutes.
type Timeout = Int

data HandshakeFailure = 
  ServerIsBusy |
  -- | 
  -- A mismatch of the internal protocol versions on client and server.
  -- First is the version on the client, second is the version on the server.
  ProtocolVersionMismatch ProtocolVersion ProtocolVersion |
  UserProtocolVersionMismatch UserProtocolVersion UserProtocolVersion |
  Unauthenticated
  deriving (Show, Generic)

instance Serializable m HandshakeFailure


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
  -- This is only expected to happen in case of user's protocol mismatch.
  CorruptRequest Text | 
  -- |
  -- A connection keepalive timeout reached.
  TimeoutReached
  deriving (Show, Generic)

instance Serializable m FailureResponse


-----------------------------

version :: ProtocolVersion
version = 1
