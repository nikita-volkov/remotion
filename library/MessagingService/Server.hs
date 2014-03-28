module MessagingService.Server
  (
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
    -- * Control
    Server,
    start,
    stop,
    startAndBlock,
  )
  where

import MessagingService.Util.Prelude hiding (listen)
import qualified MessagingService.Server.Connection as C
import qualified MessagingService.Protocol as P
import qualified MessagingService.Util.Forking as F
import qualified MessagingService.Util.FileSystem as FS
import qualified Network
import qualified Data.Set as Set


-- | The Server handle.
data Server = Server {
  -- | Wait for the server to either due to an error or after 'stop' is called on it.
  wait :: IO (),
  -- | Shut the server down, while releasing all resources.
  stop :: IO ()
}

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
-- @(Data.Text.IO.'Data.Text.IO.putStrLn' . (\"MessagingService.Server: \" `<>`))@.
type Log = Text -> IO ()


-- | 
-- Start a server with the provided settings on a separate thread and
-- return a handle to it, which can be used to stop it gracefully.
start :: (Serializable IO i, Serializable IO o) => Settings i o s -> IO Server
start (userVersion, listeningMode, timeout, maxClients, log, processRequest) = do

  let (portID, auth) = case listeningMode of
        Host port auth -> (Network.PortNumber $ fromIntegral port, auth)
        Socket path -> (Network.UnixSocket $ FS.encodeString path, const $ pure True)

  listeningSocket <- Network.listenOn portID

  slotsVar <- newMVar maxClients
  sessionThreadsVar <- newMVar Set.empty

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

  (listenThread, listenWait) <- F.forkRethrowingFinallyWithWait cleanUp listen
  return $ 
    let stop = F.killThread listenThread
        wait = void listenWait
        in Server wait stop

-- |
-- Run the server, while blocking the calling thread.
startAndBlock :: (Serializable IO i, Serializable IO o) => Settings i o s -> IO ()
startAndBlock settings = bracket (start settings) stop wait
