{-# LANGUAGE CPP #-}
module Remotion.Client (
  -- * Control
  ConnectionT,
  runConnectionT,
  request,
  -- * Settings
  Settings(..),
  URL(..),
  Protocol.Credentials(..),
  Protocol.UserProtocolVersion,
  -- * Failure
  Failure(..),
)
where


import Remotion.Util.Prelude hiding (State, listen, interact)
import qualified Remotion.Util.Prelude as Prelude
import qualified Remotion.SessionT as S
import qualified Remotion.Protocol as Protocol
import qualified Remotion.Client.InteractionT as I
import qualified Remotion.Client.Sessions as Sessions
import qualified Control.Concurrent.Async.Lifted as A
import qualified Control.Concurrent.Lock as Lock
import qualified Network
import qualified Remotion.Util.FileSystem as FS

-- |
-- A monad transformer for performing actions on client-side.
-- 
-- Supports custom protocols with @i@ being the type of the client request and
-- @o@ - the server's response.
newtype ConnectionT i o m r = 
  ConnectionT { unConnectionT :: ReaderT Env (EitherT Failure (InteractionT i o m)) r }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Failure)

type Env = (KeepaliveState, Timeout)

type KeepaliveState = MVar (Maybe UTCTime)

type Timeout = Int

type InteractionT i o = I.InteractionT (Protocol.Request i) (Protocol.Response o)

-- |
-- Settings of 'ConnectionT'.
type Settings = (URL, Protocol.UserProtocolVersion)

-- |
-- Location of the server.
data URL =
  -- | Path to the socket-file.
  Socket FilePath |
  -- | Host name, port and credentials.
  Host Text Int Protocol.Credentials

instance MonadTrans (ConnectionT i o) where
  lift = ConnectionT . lift . lift . lift

instance (MonadBase IO m) => MonadBase IO (ConnectionT i o m) where
  liftBase = ConnectionT . liftBase

instance MonadTransControl (ConnectionT i o) where
  newtype StT (ConnectionT i o) a = StT { unStT :: Either Failure a }
  liftWith runToBase = do
    state <- ConnectionT $ ask
    ConnectionT $ lift $ lift $ liftWith $ \runInteractionT -> runToBase $
      unConnectionT >>> flip runReaderT state >>> runEitherT >>> runInteractionT >=>
      I.unStT >>> fmapL adaptSessionFailure >>> join >>> StT >>> return
  restoreT base = do
    StT r <- ConnectionT $ lift $ lift $ lift $ base
    ConnectionT $ lift $ either throwError return r

instance (MonadBaseControl IO m) => MonadBaseControl IO (ConnectionT i o m) where
  newtype StM (ConnectionT i o m) a = StMT { unStMT :: ComposeSt (ConnectionT i o) m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM = defaultRestoreM unStMT

-- |
-- Run 'ConnectionT' in the base monad.
-- 
-- Requires the base monad to have a 'MonadBaseControl' instance for 'IO'.
runConnectionT :: 
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m,
   MonadBaseControl IO m) => 
  Settings -> ConnectionT i o m r -> m (Either Failure r)
runConnectionT (url, userProtocolVersion) t = 
  runEitherT $ bracketEitherT openSocket closeSocket $ \socket -> do
    timeout <- runHandshake socket
    runInteraction socket timeout
  where
    openSocket = openURLSocketIO url |> try |> liftIO >>= \case
      Right r -> return r
      Left e -> case ioeGetErrorType e of
        NoSuchThing -> left $ UnreachableURL
        _ -> $bug $ "Unexpected IOException: " <> (packText . show) e
    closeSocket socket = liftIO $ hClose socket
    
    runHandshake socket =
      S.run session settings >>= 
      hoistEither . fmapL adaptSessionFailure >>= 
      hoistEither . fmapL adaptHandshakeFailure
      where
        session = Sessions.handshake credentials userProtocolVersion
        credentials = case url of
          Socket _ -> Nothing
          Host _ _ x -> x
        settings = (socket, 10^6*1)

    runInteraction socket timeout = do
      keepaliveState <- liftIO $ newMVar =<< Just <$> getCurrentTime
      join $ fmap hoistEither $ lift $ runStack socket keepaliveState timeout $ do
        A.withAsync (t <* stopKeepalive) $ \ta -> A.withAsync keepaliveLoop $ \ka -> do
          A.waitBoth ta ka >>= \(tr, kr) -> return tr

runStack :: 
  (MonadIO m) =>
  S.Socket -> KeepaliveState -> Timeout -> ConnectionT i o m r -> m (Either Failure r)
runStack socket keepaliveState timeout t =
  unConnectionT t |>
  flip runReaderT (keepaliveState, timeout) |>
  runEitherT |>
  flip I.run (socket, 10^6*30) |>
  liftM (join . fmapL adaptSessionFailure)

openURLSocketIO :: URL -> IO Handle
openURLSocketIO = \case
  Socket file -> 
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
    Network.connectTo "" (Network.UnixSocket $ FS.encodeString file)
#else
    error "Socket used on Windows"
#endif
  Host name port _ -> 
    Network.connectTo (unpackText name) (Network.PortNumber $ fromIntegral port)

stopKeepalive :: (MonadIO m) => ConnectionT i o m ()
stopKeepalive = do
  (state, _) <- ConnectionT $ ask
  liftIO $ modifyMVar_ state $ const $ return Nothing

keepaliveLoop :: 
  (Applicative m, MonadIO m, Serializable IO o, Serializable IO i) => 
  ConnectionT i o m ()
keepaliveLoop = do
  (state, timeout) <- ConnectionT $ ask
  let loweredTimeout = floor (fromIntegral timeout * 0.9)
  (liftIO $ readMVar state) >>= \case
    Nothing -> return ()
    Just lastTime -> do
      let nextTime = microsToDiff loweredTimeout `addUTCTime` lastTime
      currentTime <- liftIO $ getCurrentTime
      if currentTime < nextTime
        then do
          liftIO $ threadDelay $ diffToMicros $ nextTime `diffUTCTime` currentTime
        else do
          checkIn
          liftIO $ threadDelay $ diffToMicros $ loweredTimeout
      keepaliveLoop

resetKeepalive :: (MonadIO m) => ConnectionT i o m ()
resetKeepalive = do
  (state, timeout) <- ConnectionT $ ask
  liftIO $ do
    time <- getCurrentTime
    modifyMVar_ state $ const $ return $ Just time

interact :: 
  (Serializable IO o, Serializable IO i, MonadIO m, Applicative m) =>
  Protocol.Request i -> ConnectionT i o m (Maybe o)
interact = 
  I.interact >>> lift >>> lift >>> ConnectionT >=> 
  either (throwError . adaptInteractionFailure) return

checkIn :: 
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m) => 
  ConnectionT i o m ()
checkIn = 
  interact Protocol.Keepalive >>= 
  maybe (return ()) ($bug "Unexpected response")

-- |
-- Send a request @i@ and receive a response @o@.
request :: 
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m) => 
  i -> ConnectionT i o m o
request a = 
  interact (Protocol.UserRequest a) >>= 
  maybe ($bug "Unexpected response") return


-- Failure
----------------------------

data Failure = 
  -- |
  -- Unable to connect to the provided url.
  UnreachableURL |
  -- |
  -- Server has too many connections already.
  ServerIsBusy |
  -- | 
  -- A mismatch of the internal protocol versions on client and server.
  -- First is the version on the client, second is the version on the server.
  ProtocolVersionMismatch Int Int |
  -- | 
  -- A mismatch of the user-supplied versions of custom protocol on client and server.
  -- First is the version on the client, second is the version on the server.
  UserProtocolVersionMismatch Int Int |
  -- |
  -- Incorrect credentials.
  Unauthenticated |
  -- |
  -- Connection got interrupted for some reason.
  ConnectionInterrupted |
  -- |
  -- Server has not responded in the required amount of time.
  ResponseTimeoutReached
  deriving (Show, Read, Ord, Eq, Generic, Data, Typeable)

adaptHandshakeFailure :: Protocol.HandshakeFailure -> Failure
adaptHandshakeFailure = \case
  Protocol.ServerIsBusy -> ServerIsBusy
  Protocol.ProtocolVersionMismatch c s -> ProtocolVersionMismatch c s
  Protocol.UserProtocolVersionMismatch c s -> UserProtocolVersionMismatch c s
  Protocol.Unauthenticated -> Unauthenticated

adaptInteractionFailure :: Protocol.InteractionFailure -> Failure
adaptInteractionFailure = \case
  Protocol.CorruptRequest t -> $bug $ "Server reports corrupt request: " <> t
  Protocol.TimeoutReached -> $bug $ "A connection keepalive timeout reached"

adaptSessionFailure :: S.Failure -> Failure
adaptSessionFailure = \case
  S.ConnectionInterrupted -> ConnectionInterrupted
  S.TimeoutReached -> ResponseTimeoutReached
  S.CorruptData t -> $bug $ "Corrupt server response: " <> t
