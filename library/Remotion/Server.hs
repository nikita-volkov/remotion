module Remotion.Server
  (
    -- * Control
    -- ** Monad-transformer
    Server,
    run,
    wait,
    countSlots,
    -- ** Simple
    runAndWait,
    -- * Settings
    Settings,
    P.UserProtocolSignature,
    ListeningMode(..),
    Port,
    C.Authenticate,
    P.Credentials,
    P.Timeout,
    MaxClients,
    Log,
    C.ProcessUserRequest,
    C.State,
    -- * Failure
    Failure(..),
  )
  where

import Remotion.Util.Prelude hiding (listen)
import qualified Remotion.Server.Connection as C
import qualified Remotion.Protocol as P
import qualified Remotion.Util.FileSystem as FS
import qualified Control.Concurrent.Async.Lifted as As
import qualified Network
import qualified Data.Set as Set



-- | Settings of how to run the server.
type Settings i o s = 
  (P.UserProtocolSignature, ListeningMode, P.Timeout, MaxClients, Log, 
   C.ProcessUserRequest i o s)

-- | Defines how to listen for connections.
data ListeningMode =
  -- | 
  -- Listen on a port with an authentication function.
  Host Port C.Authenticate |
  -- | 
  -- Listen on a socket file.
  -- Since sockets are local no authentication is needed.
  -- Works only on UNIX systems.
  Socket FilePath

-- | A port to run the server on.
type Port = Int

-- | 
-- A maximum amount of clients.
-- When this amount is reached the server rejects all the further connections.
type MaxClients = Int

-- |
-- A logging function.
-- If you want no logging, use @(const $ return ())@.
-- If you want to output to console use @Data.Text.IO.'Data.Text.IO.putStrLn'@.
-- If you want to somehow reformat the output, you're welcome: 
-- @(Data.Text.IO.'Data.Text.IO.putStrLn' . (\"Remotion.Server: \" `<>`))@.
type Log = Text -> IO ()

--------------------------------------------------------------------------------


-- API
------------------------

-- |
-- A monad transformer, which runs the server in the background.
newtype Server m a = 
  Server { unServer :: ReaderT (Wait, CountSlots) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

type Wait = IO ()
type CountSlots = IO Int

-- |
-- A Server failure.
data Failure =
  ListeningSocketIsBusy
  -- -- FIXME: implement the following
  -- -- | An IO exception has been thrown while accepting a connection socket.
  -- ConnectionSocketFailure IOException
  deriving (Show, Eq, Generic, Typeable)

-- |
-- Run the server, while automatically managing all related resources.
run :: 
  (Serializable IO i, Serializable IO o, MonadIO m) => 
  Settings i o s -> Server m a -> m (Either Failure a)
run (userVersion, listeningMode, timeout, maxClients, log, processRequest) m = runEitherT $ do

  let (portID, auth) = case listeningMode of
        Host port auth -> (Network.PortNumber $ fromIntegral port, auth)
        Socket path -> (Network.UnixSocket $ FS.encodeString path, const $ pure True)

  listeningSocket <- Network.listenOn portID |> try |> liftIO >>= \case
    Left e -> case ioeGetErrorType e of
      ResourceBusy -> left ListeningSocketIsBusy
      _ -> $bug $ "Unexpected IO error: " <> (packText . show) e
    Right r -> return r

  slotsVar <- liftIO $ newMVar maxClients

  activeListenerLock <- liftIO $ newMVar ()

  mainThreadID <- liftIO $ myThreadId

  liftIO $ log "Listening"

  -- Spawn all workers
  listenerAsyncs <- liftIO $ forM [1..(maxClients + 1)] $ \i -> 
    let 
      log' = log . (("Listener " <> packText (show i) <> ": ") <>)
      acquire = do
        (connectionSocket, _, _) <- do
          takeMVar activeListenerLock
          log' $ "Waiting for connection"
          Network.accept listeningSocket <* putMVar activeListenerLock ()
        modifyMVar_ slotsVar $ return . pred
        return connectionSocket
      release connectionSocket = do  
        log' "Releasing session's resources"
        hClose connectionSocket
        modifyMVar_ slotsVar $ return . succ
      process connectionSocket = do
        log' "Running client session"
        slots <- readMVar slotsVar
        C.runConnection connectionSocket (slots >= 0) auth timeout userVersion processRequest >>=
          either 
            (log' . ("Session failed: " <>) . packText . show) 
            (const $ log' "Session closed")
      in As.async $ forever $ do
        s <- acquire
        r <- try $ process s
        release s
        case r of
          Right r -> return r
          Left se -> if
            | Just ThreadKilled <- fromException se -> throwIO ThreadKilled
            | otherwise -> throwTo mainThreadID se
  let
    wait = do
      void $ As.waitAnyCancel listenerAsyncs
    stop = do
      log $ "Stopping server"
      forM_ listenerAsyncs As.cancel
      Network.sClose listeningSocket
      case listeningMode of
        Socket path -> FS.removeFile path
        _ -> return ()
      log $ "Stopped server"
    countSlots = readMVar slotsVar

  r <- lift $ runReaderT (unServer m) (wait, countSlots) 
  liftIO stop
  return r

-- | Block until the server stops (which should never happen).
wait :: (MonadIO m) => Server m ()
wait = Server $ ask >>= \(x, _) -> liftIO $ x

-- | Count the currently available slots for new connections.
countSlots :: (MonadIO m) => Server m Int
countSlots = Server $ ask >>= \(_, x) -> liftIO $ x

-- |
-- Run the server, while blocking the calling thread.
runAndWait :: (Serializable IO i, Serializable IO o) => Settings i o s -> IO (Either Failure ())
runAndWait settings = run settings $ wait


-- "monad-control" instances
-------------------------

instance MonadBase IO m => MonadBase IO (Server m) where
  liftBase = Server . liftBase

instance MonadTransControl Server where
  newtype StT Server a = StT { unStT :: a }
  liftWith runToM = do
    wait <- Server $ ask
    Server $ lift $ runToM $ liftM StT . flip runReaderT wait . unServer
  restoreT m = do
    StT r <- Server $ lift $ m
    return r
    
instance (MonadBaseControl IO m) => MonadBaseControl IO (Server m) where
  newtype StM (Server m) a = StMT { unStMT :: ComposeSt Server m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM = defaultRestoreM unStMT



