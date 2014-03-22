module MessagingService.Server
  (
    -- * Settings
    Settings,
    ListeningMode(..),
    Port,
    Session.Authenticate,
    Session.Credentials,
    Session.Timeout,
    MaxClients,
    Log,
    Session.ProcessMessage,
    Session.State,
    -- * Control
    Server,
    start,
    stop,
    startAndBlock,
  )
  where

import MessagingService.Util.Prelude hiding (listen)
import qualified MessagingService.Server.Session as Session
import qualified MessagingService.Util.Forking as F
import qualified MessagingService.Util.FileSystem as FS
import qualified Network
import qualified Network.Socket
import qualified Data.Set as Set


-- | The Server handle.
data Server = Server {
  -- | Wait for the server to either due to an error or after 'stop' is called on it.
  wait :: IO (),
  -- | Shut the server down, while releasing all resources.
  stop :: IO ()
}

-- | Settings of how to run the server.
type Settings i o s = (ListeningMode, Session.Timeout, MaxClients, Log, Session.ProcessMessage i o s)

-- | Defines how to listen for connections.
data ListeningMode =
  -- | 
  -- Listen on a port with an authentication function.
  ListeningMode_Host Port Session.Authenticate |
  -- | 
  -- Listen on a socket file.
  -- Since sockets are local no authentication is needed.
  -- Works only on UNIX systems.
  ListeningMode_Socket FilePath

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
start (listeningMode, timeout, maxClients, log, processMessage) = do

  let (portID, auth) = case listeningMode of
        ListeningMode_Host port auth -> (Network.PortNumber $ fromIntegral port, auth)
        ListeningMode_Socket path -> (Network.UnixSocket $ FS.encodeString path, const $ pure True)

  listeningSocket <- Network.listenOn portID

  slotsVar <- newMVar maxClients
  sessionThreadsVar <- newMVar Set.empty

  let 
    listen = forever $ modifyMVar_ slotsVar $ \slots -> do
      log "Listening"
      (connectionSocket, _) <- Network.Socket.accept listeningSocket
      log "Client connected"
      let 
        runSession sess settings = 
          Session.run sess settings >>= 
          either (liftIO . log . ("Session error: " <>) . packText . show) 
                 (const $ return ())
        registerThread = do
          tid <- F.myThreadId
          modifyMVar_ sessionThreadsVar $ return . Set.insert tid
        unregisterThread = do
          tid <- F.myThreadId
          modifyMVar_ sessionThreadsVar $ return . Set.delete tid
      if slots <= 0
        then do
          let timeout = 10^6
              settings = (connectionSocket, timeout, auth, processMessage)
              forkRethrowing = F.forkRethrowingFinally $ do
                Network.sClose connectionSocket
                unregisterThread
          forkRethrowing $ do
            registerThread
            runSession Session.sendTooManyConnections settings
          return slots
        else do
          let settings = (connectionSocket, timeout, auth, processMessage)
              forkRethrowing = F.forkRethrowingFinally $ do
                modifyMVar_ slotsVar (return . succ)
                Network.sClose connectionSocket
                unregisterThread
          forkRethrowing $ do
            registerThread
            runSession (Session.sendOkay >> Session.interact) settings
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
