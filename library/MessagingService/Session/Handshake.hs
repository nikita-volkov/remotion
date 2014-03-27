module MessagingService.Session.Handshake where

import MessagingService.Util.Prelude hiding (State)
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified MessagingService.Session as S


type Handshake = Free HandshakeF

data HandshakeF n =
  GetAvailable (Bool -> n) |
  GetClientProtocolVersion (ProtocolVersion -> n) |
  GetServerProtocolVersion (ProtocolVersion -> n) |
  GetClientUserProtocolVersion (UserProtocolVersion -> n) |
  GetServerUserProtocolVersion (UserProtocolVersion -> n) |
  GetCredentials (Credentials -> n) |
  Authenticate Credentials (Bool -> n) |
  GetTimeout (Timeout -> n)
  deriving (Functor)

-- |
-- A version of the internal protocol of \"messaging-service\"
-- used for checking of server-client match.
type ProtocolVersion = Int
-- |
-- A user-supplied version of user's protocol
-- used for checking of server-client match.
type UserProtocolVersion = Int

-- | 
-- A function, which checks the hashed authentication data.
-- If you want to provide access to anybody, use @(\_ -> return True)@.
type Authenticate = Credentials -> IO Bool

-- |
-- Either a plain ASCII password or an encoding of some data, 
-- e.g. an MD5 hash of a login-password pair or just a password.
-- 
-- @Nothing@ means anonymous.
type Credentials = Maybe ByteString

data Failure = 
  ServerIsBusy |
  -- | 
  -- A mismatch of protocol versions of \"messaging-service\" on client and server.
  -- First is the version on client, second is the version on server.
  ProtocolVersionMismatch ProtocolVersion ProtocolVersion |
  UserProtocolVersionMismatch UserProtocolVersion UserProtocolVersion |
  Unauthenticated
  deriving (Show)

-- |
-- A session timeout in microseconds. 
-- The period of keepalive signaling depends on that parameter.
-- If you don't want excessive requests, just make it a couple of minutes.
type Timeout = Int

-- Generate actions.
makeFree ''HandshakeF

version :: ProtocolVersion
version = 1

runServerSide ::
  (MonadIO m, Applicative m) =>
  Bool ->
  Authenticate ->
  Timeout ->
  Handshake a ->
  S.SessionT m a
runServerSide available authenticate timeout = \case
  Free free -> case free of
    GetAvailable c -> do
      S.send available
      continue $ c available
    GetClientProtocolVersion c -> S.receive >>= continue . c
    -- ...
    Authenticate credentials c -> do
      ok <- liftIO $ authenticate $ credentials
      S.send ok
      continue $ c ok
    GetTimeout c -> do
      S.send timeout
      continue $ c timeout
  Pure a -> pure a
  where
    continue = runServerSide available authenticate timeout

runClientSide ::
  (MonadIO m, Applicative m) =>
  UserProtocolVersion ->
  Credentials ->
  Handshake a ->
  S.SessionT m a
runClientSide userVersion credentials = \case
  Free free -> case free of
    GetAvailable c -> S.receive >>= continue . c
    GetClientProtocolVersion c -> S.send version >> pure version >>= continue . c
    GetServerProtocolVersion c -> S.receive >>= continue . c
    GetClientUserProtocolVersion c -> S.send userVersion >> continue (c userVersion)
    GetServerUserProtocolVersion c -> S.receive >>= continue . c
    GetCredentials c -> S.send credentials >> continue (c credentials)
    Authenticate credentials c -> S.receive >>= continue . c
    GetTimeout c -> S.receive >>= continue . c
  Pure a -> return a
  where
    continue = runClientSide userVersion credentials
  
standard :: Handshake (Either Failure Timeout)
standard = runEitherT $ do
  do 
    available <- lift $ getAvailable
    when (not available) $ left $ ServerIsBusy
  do
    cv <- lift $ getClientProtocolVersion
    sv <- lift $ getServerProtocolVersion
    when (cv /= sv) $ left $ ProtocolVersionMismatch cv sv
  do
    cv <- lift $ getClientUserProtocolVersion
    sv <- lift $ getServerUserProtocolVersion
    when (cv /= sv) $ left $ UserProtocolVersionMismatch cv sv
  do
    credentials <- lift $ getCredentials
    ok <- lift $ authenticate credentials
    when (not ok) $ left Unauthenticated
  do
    lift $ getTimeout
