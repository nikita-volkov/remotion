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


import Remotion.Util.Prelude hiding (traceIO, traceIOWithTime, State, listen, interact)
import qualified Remotion.Util.Prelude as Prelude
import qualified Remotion.SessionT as S
import qualified Remotion.Protocol as Protocol
import qualified Remotion.Client.Sessions as Sessions
import qualified Control.Concurrent.Async.Lifted as A
import qualified Control.Concurrent.Lock as Lock
import qualified Network
import qualified Remotion.Util.FileSystem as FS


-- Debugging.
-------------------------
-- The following functions get enabled during debugging.

debugging = True
prefix = ("Client: " <>)
traceIO = if debugging 
  then Prelude.traceIO . prefix 
  else const $ return ()
traceIOWithTime = if debugging 
  then Prelude.traceIOWithTime . prefix 
  else const $ return ()

--------------------------------------------------------------------------------

-- |
-- A monad transformer for performing actions on client-side.
-- 
-- Supports custom protocols with @i@ being the type of the client request and
-- @o@ - the server's response.
newtype ConnectionT i o m r = 
  ConnectionT { unConnectionT :: ReaderT Env (EitherT Failure (S.SessionT m)) r }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Failure)

type Env = (KeepaliveState, KeepaliveTimeout, Lock)

type KeepaliveState = MVar (Maybe UTCTime)

type KeepaliveTimeout = Int

-- | Ensures a response to request accomodation in concurrency.
type Lock = Lock.Lock


-- |
-- Settings of 'ConnectionT'.
type Settings = (Protocol.UserProtocolVersion, URL)

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
  newtype StT (ConnectionT i o) a = StT (StT S.SessionT (Either Failure a))
  liftWith runInM = do
    env <- ConnectionT $ ask
    ConnectionT $ lift $ lift $ liftWith $ \runSessionT -> runInM $ 
      liftM StT . runSessionT . runEitherT . flip runReaderT env . unConnectionT
  restoreT m = do
    r <- ConnectionT $ lift $ lift $ do
      StT r <- lift m
      restoreT $ return $ r
    either throwError return r

instance (MonadBaseControl IO m) => MonadBaseControl IO (ConnectionT i o m) where
  newtype StM (ConnectionT i o m) a = StMT { unStMT :: ComposeSt (ConnectionT i o) m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM = defaultRestoreM unStMT

liftSessionT :: (Monad m) => S.SessionT m a -> ConnectionT i o m a
liftSessionT s = ConnectionT $ lift $ do
  r <- lift $ catchError (liftM Right $ s) (return . Left . adaptSessionFailure)
  hoistEither r


-- |
-- Run 'ConnectionT' in the base monad.
-- 
-- Requires the base monad to have a 'MonadBaseControl' instance for 'IO'.
runConnectionT :: 
  forall i o m r.
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m,
   MonadBaseControl IO m) => 
  Settings -> ConnectionT i o m r -> m (Either Failure r)
runConnectionT (userProtocolVersion, url) t = 
  runEitherT $ bracketME openSocket closeSocket $ \socket -> do
    timeout <- runHandshake socket
    lock <- liftIO $ Lock.new
    runInteraction socket timeout lock
  where
    openSocket = openURLSocketIO url |> try |> liftIO >>= \case
      Right r -> return r
      Left e -> case ioeGetErrorType e of
        NoSuchThing -> left $ UnreachableURL
        _ -> $bug $ "Unexpected IOException: " <> (packText . show) e
    closeSocket socket = do
      traceIOWithTime $ "Closing socket " <> show socket
      liftIO $ handle (const $ return () :: SomeException -> IO ()) $ hClose socket
    runHandshake socket =
      S.run session settings >>= 
      hoistEither . fmapL adaptSessionFailure >>= 
      hoistEither . fmapL adaptHandshakeFailure
      where
        session = Sessions.handshake credentials userProtocolVersion
        credentials = case url of
          Socket _ -> Nothing
          Host _ _ x -> x
        settings = (socket, 10^6*3)
    runInteraction socket timeout lock = do
      traceIOWithTime $ "Interacting on socket " <> show socket
      keepaliveState <- liftIO $ newMVar =<< Just <$> getCurrentTime
      join $ fmap hoistEither $ lift $ runStack socket keepaliveState timeout lock $ do
        A.withAsync (finallyME (t <* closeSession) stopKeepalive) $ \ta ->
          A.withAsync (keepaliveLoop) $ \ka -> do
            A.waitBoth ta ka >>= \(tr, kr) -> return tr

runStack :: 
  (MonadIO m) =>
  S.Socket -> KeepaliveState -> KeepaliveTimeout -> Lock -> ConnectionT i o m r -> m (Either Failure r)
runStack socket keepaliveState timeout lock t =
  unConnectionT t |>
  flip runReaderT (keepaliveState, timeout, lock) |>
  runEitherT |>
  flip S.run (socket, 10^6*30) |>
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
  traceIOWithTime "Stopping keepalive"
  (state, _, _) <- ConnectionT $ ask
  liftIO $ modifyMVar_ state $ const $ return Nothing
  traceIOWithTime "Stopped keepalive"

keepaliveLoop :: 
  (Applicative m, MonadIO m, Serializable IO o, Serializable IO i) => 
  ConnectionT i o m ()
keepaliveLoop = do
  (state, timeout, _) <- ConnectionT $ ask
  let loweredTimeout = (floor . reduceTimeout . fromIntegral) timeout
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
          liftIO $ threadDelay $ fromIntegral $ loweredTimeout
      keepaliveLoop

reduceTimeout :: Double -> Double
reduceTimeout = curve 1 2
  where
    curve bending startingStraightness x = x / exp (bending / (x + startingStraightness))

resetKeepalive :: (MonadIO m) => ConnectionT i o m ()
resetKeepalive = do
  (state, timeout, _) <- ConnectionT $ ask
  liftIO $ do
    time <- getCurrentTime
    modifyMVar_ state $ const $ return $ Just time

interact :: 
  (Serializable IO o, Serializable IO i, MonadIO m, Applicative m) =>
  Protocol.Request i -> ConnectionT i o m (Maybe o)
interact = \request -> do
  withLock $ send request >> receive >>= either (throwError . adaptInteractionFailure) return
  where
    withLock action = do
      (_, _, l) <- ConnectionT ask
      lock l
      finallyME action (unlock l)
      where
        lock = ConnectionT . liftIO . Lock.acquire
        unlock = ConnectionT . liftIO . Lock.release
    send r = 
      traceIOWithTime "Sending" *>
      (liftSessionT $ S.send r) <*
      traceIOWithTime "Sent"
    receive = 
      traceIOWithTime "Receiving" *>
      liftSessionT S.receive <*
      traceIOWithTime "Received"

checkIn :: 
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m) => 
  ConnectionT i o m ()
checkIn = 
  interact Protocol.Keepalive >>= 
  maybe (return ()) ($bug "Unexpected response")

closeSession ::
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m) => 
  ConnectionT i o m ()
closeSession =
  traceIOWithTime "Closing session" >>
  interact Protocol.CloseSession >>=
  maybe (return ()) ($bug "Unexpected response") >>
  traceIOWithTime "Closed session"

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
  ResponseTimeoutReached Int |
  -- |
  -- The request could not get sent in the required amount of time.
  RequestTimeoutReached Int
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
  Protocol.TimeoutReached t -> $bug $ "A connection keepalive timeout reached: " <> (packText . show) t

adaptSessionFailure :: S.Failure -> Failure
adaptSessionFailure = \case
  S.ConnectionInterrupted -> ConnectionInterrupted
  S.SendTimeoutReached t -> RequestTimeoutReached t
  S.ReceiveTimeoutReached t -> ResponseTimeoutReached t
  S.CorruptData t -> $bug $ "Corrupt server response: " <> t
