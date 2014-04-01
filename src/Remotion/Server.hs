module Remotion.Server
  (
    -- * Control
    -- ** Monad-transformer
    ServeT,
    runServeT,
    wait,
    -- ** Simple
    runAndWait,
    -- * Settings
    Settings,
    P.UserProtocolVersion,
    ListeningMode(..),
    Port,
    C.Authenticate,
    P.Credentials,
    P.Timeout,
    MaxClients,
    Log,
    C.ProcessUserRequest,
    C.State,
  )
  where

import Remotion.Util.Prelude hiding (listen)
import qualified Remotion.Server.Connection as C
import qualified Remotion.Protocol as P
import qualified Remotion.Util.Forking as F
import qualified Remotion.Util.FileSystem as FS
import qualified Network
import qualified Data.Set as Set



-- | Settings of how to run the server.
type Settings i o s = 
  (P.UserProtocolVersion, ListeningMode, P.Timeout, MaxClients, Log, 
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
-- If you want no logging, use @('Control.Monad.void' . return)@, 
-- which is a fancy way of alternatively saying @(\\_ -> return ())@.
-- If you want to output to console use @Data.Text.IO.'Data.Text.IO.putStrLn'@.
-- If you want to somehow reformat the output, you're welcome: 
-- @(Data.Text.IO.'Data.Text.IO.putStrLn' . (\"Remotion.Server: \" `<>`))@.
type Log = Text -> IO ()

--------------------------------------------------------------------------------


-- API
------------------------

-- |
-- A monad transformer, which runs the server in the background.
newtype ServeT m a = 
  ServeT { unServeT :: ReaderT Wait m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

type Wait = IO ()

-- |
-- Run the server, while automatically managing all related resources.
runServeT :: 
  (Serializable IO i, Serializable IO o, MonadIO m) => 
  Settings i o s -> ServeT m a -> m a
runServeT (userVersion, listeningMode, timeout, maxClients, log, processRequest) m = do

  let (portID, auth) = case listeningMode of
        Host port auth -> (Network.PortNumber $ fromIntegral port, auth)
        Socket path -> (Network.UnixSocket $ FS.encodeString path, const $ pure True)

  listeningSocket <- liftIO $ Network.listenOn portID

  slotsVar <- liftIO $ newMVar maxClients
  sessionThreadsVar <- liftIO $ newMVar Set.empty

  let 
    listen = forever $ modifyMVar_ slotsVar $ \slots -> do
      log "Listening"
      (connectionSocket, _, _) <- Network.accept listeningSocket
      log "Client connected"
      let 
        available = slots > 0
        forkRethrowing = F.forkRethrowingFinally $ do
          modifyMVar_ slotsVar (return . succ)
          hClose connectionSocket
          unregisterThread
        registerThread = do
          tid <- F.myThreadId
          modifyMVar_ sessionThreadsVar $ return . Set.insert tid
        unregisterThread = do
          tid <- F.myThreadId
          modifyMVar_ sessionThreadsVar $ return . Set.delete tid
        runConnection =
          C.runConnection connectionSocket available auth timeout userVersion processRequest >>=
          either (liftIO . log . ("Session error: " <>) . packText . show) (const $ return ())
        in 
          forkRethrowing $ do
            registerThread
            runConnection
      return $ slots - 1
    cleanUp = do
      takeMVar sessionThreadsVar >>= mapM_ F.killThread
      Network.sClose listeningSocket

  (listenThread, listenWait) <- liftIO $ F.forkRethrowingFinallyWithWait cleanUp listen
  
  let 
    wait = void $ listenWait
    stop = F.killThread listenThread
  r <- unServeT m |> flip runReaderT wait
  liftIO $ stop
  return r
  
-- | Block until the server stops due to an error.
wait :: (MonadIO m) => ServeT m ()
wait = ServeT $ ask >>= liftIO

-- |
-- Run the server, while blocking the calling thread.
runAndWait :: (Serializable IO i, Serializable IO o) => Settings i o s -> IO ()
runAndWait settings = runServeT settings $ wait


-- "monad-control" instances
-------------------------

instance MonadBase IO m => MonadBase IO (ServeT m) where
  liftBase = ServeT . liftBase

instance MonadTransControl ServeT where
  newtype StT ServeT a = StT { unStT :: a }
  liftWith runToM = do
    wait <- ServeT $ ask
    ServeT $ lift $ runToM $ liftM StT . flip runReaderT wait . unServeT
  restoreT m = do
    StT r <- ServeT $ lift $ m
    return r
    
instance (MonadBaseControl IO m) => MonadBaseControl IO (ServeT m) where
  newtype StM (ServeT m) a = StMT { unStMT :: ComposeSt ServeT m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM = defaultRestoreM unStMT



