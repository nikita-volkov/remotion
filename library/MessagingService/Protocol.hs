module MessagingService.Protocol where

import MessagingService.Util.Prelude


data Request a =
  Request_Session (Request_Session_Spec a) |
  -- | Start a session, maybe passing a hash of authentication data.
  Request_StartSession (Maybe ByteString)
  deriving (Generic)

instance (Serializable m a) => Serializable m (Request a)

data Request_Session_Spec a =
  Request_Session_Spec_Message a |
  Request_Session_Spec_CheckIn |
  Request_Session_Spec_Close
  deriving (Generic)

instance (Serializable m a) => Serializable m (Request_Session_Spec a)


data Response a =
  Response_Session (Either (Response_Session_Failure) (Response_Session_Spec a)) |
  Response_StartSession Bool
  deriving (Generic)

instance (Serializable m a) => Serializable m (Response a)

data Response_Session_Failure =
  -- | Not authenticated or the client has not even yet sent a StartSession request.
  Response_Session_Failure_Unauthenticated |
  -- -- | The server is busy and suggests to retry the same request after the specified 
  -- -- amount of microseconds.
  -- Response_Session_Failure_Busy Int |
  -- | The session got closed due to a keepalive timeout.
  Response_Session_Failure_Timeout
  deriving (Generic)

instance Serializable m Response_Session_Failure

data Response_Session_Spec a =
  Response_Session_Spec_Message a |
  Response_Session_Spec_CheckIn |
  Response_Session_Spec_Close
  deriving (Generic)

instance (Serializable m a) => Serializable m (Response_Session_Spec a)

