module Remotion.Protocol where

import Remotion.Util.Prelude


-- Handshake
-----------------------------

-- |
-- A version of the internal protocol used for checking of server-client match.
type ProtocolVersion = Int

-- |
-- A unique identification of user's protocol version used for checking
-- of protocol versions mismatch between client and server.
-- It can be simply a user-supplied version number or 
-- a hash or a serialization of the representation of a type used for protocol,
-- which can be generated using such library as 
-- <http://hackage.haskell.org/package/type-structure type-structure>.
type UserProtocolSignature = ByteString

-- |
-- Either a plain ASCII password or an encoding of some data, 
-- e.g. an MD5 hash of a username-password pair or just a password.
-- In more involved scenarios you can mix in serialization, 
-- e.g. a serialized pair of username and a hash of just the password.
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
  UserProtocolSignatureMismatch UserProtocolSignature UserProtocolSignature |
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

type Response a = Either InteractionFailure (Maybe a)

-- |
-- A failure response from server.
data InteractionFailure = 
  -- | 
  -- Server was unable to deserialize the request.
  -- This is only expected to happen in case of user's protocol mismatch.
  CorruptRequest Text | 
  -- |
  -- A connection keepalive timeout reached.
  TimeoutReached Int
  deriving (Show, Generic)

instance Serializable m InteractionFailure


-----------------------------

version :: ProtocolVersion
version = 1
