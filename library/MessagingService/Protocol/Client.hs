module MessagingService.Protocol.Client where

import MessagingService.Util.Prelude


data Message a =
  Authenticate (Maybe ByteString) |
  CloseSession |
  Ping |
  Data a
  deriving (Generic, Show)

instance Serializable m a => Serializable m (Message a)
