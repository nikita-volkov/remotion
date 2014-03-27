{-# LANGUAGE CPP #-}
module MessagingService.Client (
  ConnectionT,
  -- ** Failure
  Failure(..),
  Handshake.ProtocolVersion,
  -- ** Execution and Configuration
  runConnectionT,
  Settings(..),
  URL(..),
  Handshake.Credentials(..),
  Handshake.UserProtocolVersion,
  -- ** Interaction
  request,
)
where


import MessagingService.Util.Prelude hiding (State, listen, interact)
import qualified MessagingService.Util.Prelude as Prelude
import qualified MessagingService.Session as S
import qualified MessagingService.Session.Handshake as Handshake
import qualified MessagingService.Session.Interaction as Interaction
import qualified MessagingService.Client.InteractionT as I
import qualified Control.Concurrent.Async.Lifted as A
import qualified Control.Concurrent.Lock as Lock
import qualified Network
import qualified MessagingService.Util.FileSystem as FS

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

type InteractionT i o = I.InteractionT (Interaction.Request i) (Interaction.Response o)

-- |
-- Settings of 'ConnectionT'.
type Settings = (URL, Handshake.Credentials, Handshake.UserProtocolVersion)

-- |
-- Location of the server.
data URL =
  -- | Path to the socket-file.
  Socket FilePath |
  -- | Host name, port and credentials.
  Host Text Int Handshake.Credentials

data Failure = 
  -- |
  -- Unable to connect to the provided url.
  UnreachableURL |
  -- |
  -- A failure during the handshake phase.
  HandshakeFailure Handshake.Failure | 
  -- |
  -- A server-side failure concerning this connection.
  InteractionFailure Interaction.Failure | 
  -- |
  -- A client-side failure related to connection bookkeeping.
  SessionFailure S.Failure
  deriving (Show)

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
      I.unStT >>> fmapL SessionFailure >>> join >>> StT >>> return
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
runConnectionT (url, credentials, userProtocolVersion) t = 
  runEitherT $ bracketEitherT openSocket closeSocket $ \socket -> do
    timeout <- runHandshake socket
    runInteraction socket timeout
  where
    openSocket = openURLSocketIO url |> try |> liftIO >>= \case
      Right r -> return r
      Left e -> case ioeGetErrorType e of
        NoSuchThing -> left $ UnreachableURL
        _ -> $bug $ "Unexpected IOException: " <> show e
    closeSocket socket = liftIO $ hClose socket
    
    runHandshake socket =
      S.run session settings >>= 
      hoistEither . fmapL SessionFailure >>= 
      hoistEither . fmapL HandshakeFailure
      where
        session = Handshake.runClientSide userProtocolVersion credentials Handshake.standard
        settings = (socket, 10^6*1)

    runInteraction socket timeout = do
      keepaliveState <- liftIO $ newMVar =<< Just <$> getCurrentTime
      join $ fmap hoistEither $ lift $ runStack socket keepaliveState timeout $ do
        A.withAsync t $ \ta -> A.withAsync keepaliveLoop $ \ka -> do
          A.waitBoth ta ka >>= \(tr, kr) -> return tr

runStack :: 
  (MonadIO m) =>
  S.Socket -> KeepaliveState -> Timeout -> ConnectionT i o m r -> m (Either Failure r)
runStack socket keepaliveState timeout t =
  unConnectionT t |>
  flip runReaderT (keepaliveState, timeout) |>
  runEitherT |>
  flip I.run (socket, 10^6*30) |>
  liftM (join . fmapL SessionFailure)

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
  Interaction.Request i -> ConnectionT i o m (Maybe o)
interact = 
  I.interact >>> lift >>> lift >>> ConnectionT >=> 
  either (throwError . InteractionFailure) return

checkIn :: 
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m) => 
  ConnectionT i o m ()
checkIn = 
  interact Interaction.Keepalive >>= 
  maybe (return ()) ($bug "Unexpected response")

-- |
-- Perform a custom request.
request :: 
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m) => 
  i -> ConnectionT i o m o
request a = 
  interact (Interaction.UserRequest a) >>= 
  maybe ($bug "Unexpected response") return
