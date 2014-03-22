module MessagingService.Protocol.Request where

import MessagingService.Util.Prelude


type Request a = Either Command a
data Command = 
  Authenticate (Maybe ByteString) |
  CloseSession |
  Ping
  deriving (Generic)

instance Serializable m Command
