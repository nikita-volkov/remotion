module MessagingService.Handshake where

import MessagingService.Util.Prelude
import qualified MessagingService.ConnectionT as C

type HandshakeT m = EitherT Failure (C.ConnectionT m)

type ProtocolVersion = Int
type UserProtocolVersion = Int
type Credentials = Maybe ByteString

data Failure = 
  ServerIsBusy |
  -- | 
  -- A mismatch of protocol versions of \"messaging-service\" on client and server.
  -- First is the version on client, second is the version on server.
  ProtocolVersionMismatch ProtocolVersion ProtocolVersion |
  UserProtocolVersionMismatch UserProtocolVersion UserProtocolVersion |
  Unauthenticated

version :: ProtocolVersion
version = 1

send = lift . C.send
receive = lift C.receive

client :: 
  (MonadIO m, Applicative m) =>
  UserProtocolVersion -> 
  Credentials -> 
  HandshakeT m ()
client upv credentials = do
  do
    available <- receive
    when (not available) $ left $ ServerIsBusy
  do
    send $ version
    mismatch <- receive
    case mismatch of
      Just pv' -> left $ ProtocolVersionMismatch version pv'
      Nothing -> return ()
  do
    send $ upv
    mismatch <- receive
    case mismatch of
      Just upv' -> left $ UserProtocolVersionMismatch upv upv'
      Nothing -> return ()
  do
    send $ credentials
    ok <- receive
    when (not ok) $ left $ Unauthenticated

server available auth = do
  do
    send $ available
    when (not available) $ left $ ServerIsBusy
  do
    pv' <- receive
    if pv' /= version
      then do
        send $ Just $ version
        left $ ProtocolVersionMismatch pv' version
      else do
        send $ (Nothing :: Maybe ProtocolVersion)
  $(todo "Check user version")
  do
    credentials <- receive
    ok <- liftIO $ auth credentials
    send ok
    when (not ok) $ left $ Unauthenticated

