module MessagingService.Protocol.Response where

import MessagingService.Util.Prelude


type Response a = Either Failure (Maybe a, Timeout)

data Failure = 
  Unauthenticated | TimeoutReached | CorruptRequest Text | TooManyConnections
  deriving (Generic)

instance Serializable m Failure

-- | An allowed timeout till the next request is required in μs.
type Timeout = Int



