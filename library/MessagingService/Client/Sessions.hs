module MessagingService.Client.Sessions where

import MessagingService.Util.Prelude
import qualified MessagingService.SessionT as S
import qualified MessagingService.Protocol as P


handshake ::
  (MonadIO m, Applicative m) =>
  P.Credentials ->
  P.UserProtocolVersion ->
  S.SessionT m (Either P.HandshakeFailure P.Timeout)
handshake credentials userProtocolVersion = runEitherT $ do
  do
    receiveFailure
  do
    send P.version
    receiveFailure
  do
    send userProtocolVersion
    receiveFailure
  do
    send credentials
    receiveFailure
  do
    receive
  where
    send = lift . S.send
    receive = lift S.receive
    receiveFailure = receive >>= maybe (return ()) left

