module MessagingService.Server.ClientState where

import MessagingService.Util.Prelude


data ClientState sessionData = ClientState {
  id :: Int,
  authenticated :: Bool,
  lastCheckIn :: UTCTime,
  sessionData :: sessionData
}


