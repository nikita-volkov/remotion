module MessagingService.Protocol.Server where

import MessagingService.Util.Prelude


data Message a = 
  Welcome Timeout |
  TooManyConnections |
  Unauthenticated |
  TimeoutReached | 
  CorruptRequest Text |
  Okay (Maybe a)
  deriving (Show, Generic)

-- | An allowed timeout till the next request is required in μs.
type Timeout = Int

instance Serializable m a => Serializable m (Message a)



