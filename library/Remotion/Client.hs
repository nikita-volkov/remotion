{-# LANGUAGE CPP #-}
module Remotion.Client (
  -- * Control
  Client,
  runClient,
  request,
  -- * Settings
  Settings(..),
  URL(..),
  P.Credentials(..),
  P.UserProtocolVersion,
  -- * Failure
  Failure(..),
)
where


import Remotion.Util.Prelude hiding (traceIO, traceIOWithTime, State, listen, interact)
import qualified Remotion.Util.Prelude as Prelude
import qualified Remotion.Session as S
import qualified Remotion.Protocol as P
import qualified Control.Concurrent.Async.Lifted as A
import qualified Control.Concurrent.Lock as Lock
import qualified Network
import qualified Remotion.Util.FileSystem as FS


-- Debugging.
-------------------------
-- The following functions get enabled during debugging.

debugging = False
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
newtype Client i o m r = 
  Client { unClient :: ReaderT Env (EitherT Failure (S.Session m)) r }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Failure)

type Env = (KeepaliveState, KeepaliveTimeout, Lock)

type KeepaliveState = MVar (Maybe UTCTime)

type KeepaliveTimeout = Int

-- | Ensures a response to request accomodation in concurrency.
type Lock = Lock.Lock


-- |
-- Settings of 'Client'.
type Settings = (P.UserProtocolVersion, URL)

-- |
-- Location of the server.
data URL =
  -- | Path to the socket-file.
  Socket FilePath |
  -- | Host name, port and credentials.
  Host Text Int P.Credentials

instance MonadTrans (Client i o) where
  lift = Client . lift . lift . lift

instance (MonadBase IO m) => MonadBase IO (Client i o m) where
  liftBase = Client . liftBase

instance MonadTransControl (Client i o) where
  newtype StT (Client i o) a = StT (StT S.Session (Either Failure a))
  liftWith runInM = do
    env <- Client $ ask
    Client $ lift $ lift $ liftWith $ \runSession -> runInM $ 
      liftM StT . runSession . runEitherT . flip runReaderT env . unClient
  restoreT m = do
    r <- Client $ lift $ lift $ do
      StT r <- lift m
      restoreT $ return $ r
    either throwError return r

instance (MonadBaseControl IO m) => MonadBaseControl IO (Client i o m) where
  newtype StM (Client i o m) a = StMT { unStMT :: ComposeSt (Client i o) m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM = defaultRestoreM unStMT

liftSession :: (Monad m) => S.Session m a -> Client i o m a
liftSession s = Client $ lift $ do
  r <- lift $ catchError (liftM Right $ s) (return . Left . adaptSessionFailure)
  hoistEither r


-- |
-- Run 'Client' in the base monad.
-- 
-- Requires the base monad to have a 'MonadBaseControl' instance for 'IO'.
runClient :: 
  forall i o m r.
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m,
   MonadBaseControl IO m) => 
  Settings -> Client i o m r -> m (Either Failure r)
runClient (userProtocolVersion, url) t = 
  runEitherT $ bracketME openSocket closeSocket $ \socket -> do
    timeout <- runHandshake socket
    lock <- liftIO $ Lock.new
    runInteraction socket timeout lock
  where
    openSocket = do
      traceIOWithTime "Opening socket"
      openURLSocketIO url |> try |> liftIO >>= \case
        Right r -> return r
        Left e -> case ioeGetErrorType e of
          NoSuchThing -> left $ UnreachableURL
          _ -> $bug $ "Unexpected IOException: " <> (packText . show) e
    closeSocket socket = do
      traceIOWithTime $ "Closing socket " <> show socket
      liftIO $ handle (const $ return () :: SomeException -> IO ()) $ hClose socket
    runHandshake socket =
      traceIOWithTime "Handshaking" >>
      S.run session settings >>= 
      hoistEither . fmapL adaptSessionFailure >>= 
      hoistEither . fmapL adaptHandshakeFailure
      where
        session = runEitherT $ do
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
            send (0::Int)
            receive
          where
            send = lift . S.send
            receive = lift S.receive
            receiveFailure = receive >>= maybe (return ()) left
        credentials = case url of
          Socket _ -> Nothing
          Host _ _ x -> x
        settings = (socket, 10^6*3)
    runInteraction socket timeout lock = do
      traceIOWithTime $ "Interacting"
      keepaliveState <- liftIO $ newMVar Nothing
      join $ fmap hoistEither $ lift $ runStack socket keepaliveState timeout lock $ do
        A.withAsync (finallyME (resetKeepalive *> t <* closeSession) stopKeepalive) $ \ta ->
          A.withAsync (keepaliveLoop) $ \ka -> do
            A.waitBoth ta ka >>= \(tr, kr) -> return tr

runStack :: 
  (MonadIO m) =>
  S.Socket -> KeepaliveState -> KeepaliveTimeout -> Lock -> Client i o m r -> m (Either Failure r)
runStack socket keepaliveState keepaliveTimeout lock t =
  if keepaliveTimeout < 10^3*100
    then error $ "Too small keepalive timeout setting: " <> show keepaliveTimeout
    else
      unClient t |>
      flip runReaderT (keepaliveState, keepaliveTimeout, lock) |>
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

stopKeepalive :: (MonadIO m) => Client i o m ()
stopKeepalive = do
  traceIOWithTime "Stopping keepalive"
  (state, _, _) <- Client $ ask
  liftIO $ modifyMVar_ state $ const $ return Nothing

keepaliveLoop :: 
  (Applicative m, MonadIO m, Serializable IO o, Serializable IO i) => 
  Client i o m ()
keepaliveLoop = do
  (state, _, _) <- Client $ ask
  (liftIO $ readMVar state) >>= \case
    Nothing -> return ()
    Just nextTime -> do
      currentTime <- liftIO $ getCurrentTime
      when (currentTime >= nextTime) $ checkIn
      liftIO $ threadDelay $ 10^3 * 10
      keepaliveLoop

reduceTimeout :: Int -> Int
reduceTimeout = floor . (*10^6) . curve 1.2 1.3 . (/(10^6)) . fromIntegral
  where
    curve bending startingStraightness x = x / exp (bending / (x + startingStraightness))

resetKeepalive :: (MonadIO m) => Client i o m ()
resetKeepalive = do
  (state, timeout, _) <- Client $ ask
  liftIO $ do
    time <- getCurrentTime
    let nextTime = (microsToDiff $ toInteger $ reduceTimeout timeout) `addUTCTime` time
    modifyMVar_ state $ const $ return $ Just $ nextTime
      
interact :: 
  (Serializable IO o, Serializable IO i, MonadIO m, Applicative m) =>
  P.Request i -> Client i o m (Maybe o)
interact = \request -> do
  withLock $ send request >> receive >>= either (\f -> throwError $! adaptInteractionFailure f) return
  where
    withLock action = do
      (_, _, l) <- Client ask
      lock l
      finallyME action (unlock l)
      where
        lock = Client . liftIO . Lock.acquire
        unlock = Client . liftIO . Lock.release
    send r = 
      traceIOWithTime "Sending" *>
      (liftSession $ S.send r)
    receive = 
      traceIOWithTime "Receiving" *>
      liftSession S.receive

checkIn :: 
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m) => 
  Client i o m ()
checkIn = do 
  traceIOWithTime "Performing keepalive request"
  resetKeepalive
  interact P.Keepalive >>= maybe (return ()) ($bug "Unexpected response")

closeSession ::
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m) => 
  Client i o m ()
closeSession =
  traceIOWithTime "Closing session" >>
  interact P.CloseSession >>=
  maybe (return ()) ($bug "Unexpected response")

-- |
-- Send a request @i@ and receive a response @o@.
request :: 
  (Serializable IO i, Serializable IO o, MonadIO m, Applicative m) => 
  i -> Client i o m o
request a = do
  resetKeepalive
  interact (P.UserRequest a) >>= maybe ($bug "Unexpected response") return


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
  RequestTimeoutReached Int |
  -- |
  -- Server reports corrupt request.
  CorruptRequest Text
  deriving (Show, Read, Ord, Eq, Generic, Data, Typeable)

adaptHandshakeFailure :: P.HandshakeFailure -> Failure
adaptHandshakeFailure = \case
  P.ServerIsBusy -> ServerIsBusy
  P.ProtocolVersionMismatch c s -> ProtocolVersionMismatch c s
  P.UserProtocolVersionMismatch c s -> UserProtocolVersionMismatch c s
  P.Unauthenticated -> Unauthenticated

adaptInteractionFailure :: P.InteractionFailure -> Failure
adaptInteractionFailure = \case
  P.CorruptRequest t -> CorruptRequest t
  P.TimeoutReached t -> $bug $ "A connection keepalive timeout reached: " <> (packText . show) t

adaptSessionFailure :: S.Failure -> Failure
adaptSessionFailure = \case
  S.ConnectionInterrupted -> ConnectionInterrupted
  S.SendTimeoutReached t -> RequestTimeoutReached t
  S.ReceiveTimeoutReached t -> ResponseTimeoutReached t
  S.CorruptData t -> $bug $ "Corrupt server response: " <> t
