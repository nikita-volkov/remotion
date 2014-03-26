module MessagingService.Protocol where

import MessagingService.Util.Prelude


type ServerIsAvailable = Bool

type ProtocolVersion = Int

type UserProtocolVersion = Int

type Credentials = Maybe ByteString

type Timeout = Int

data Request a = 
  Keepalive | 
  CloseSession |
  UserRequest a
  deriving (Generic)

instance (Serializable m a) => Serializable m (Request a)

type Response a = Either Failure (Maybe a)

data Failure = 
  CorruptData Text | 
  TimeoutReached
  deriving (Show, Generic)

instance Serializable m Failure


version :: ProtocolVersion
version = 1

