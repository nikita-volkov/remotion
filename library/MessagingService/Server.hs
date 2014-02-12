module MessagingService.Server
  (
    start,
    Settings,
    ListeningMode(..),
    Port,
    Session.Authenticate,
    Session.Hash,
    Session.Timeout,
    Log,
    Session.ProcessMessage,
    Session.State,
    Stop,
  )
  where

import MessagingService.Util.Prelude hiding (listen)
import qualified MessagingService.Server.Session as Session
import qualified MessagingService.Util.FileSystem as FS
import qualified Network
import qualified Network.Socket


-- | 
-- Start the server with the provided settings,
-- returning a function to stop it, releasing all resources.
-- 
-- Why the `CIO` monad? Because composition, that's why!
-- You can run multiple servers with a shared thread pool,
-- meaning a shared connection pool. 
-- You can compose them with whatever other concurrent things you do, 
-- but still share a thread pool.
-- 
start :: (Serializable IO i, Serializable IO o) => Settings i o s -> CIO Stop
start (listeningMode, timeout, log, processMessage) = do

  let (portID, auth) = case listeningMode of
        ListeningMode_Host port auth -> (Network.PortNumber $ fromIntegral port, auth)
        ListeningMode_Socket path -> (Network.UnixSocket $ FS.encodeString path, const $ pure True)

  listeningSocket <- liftIO $ Network.listenOn portID

  let finalizer = liftIO $ Network.sClose listeningSocket
  listenerFork <- forkFinallyCIO finalizer $ do
    forever $ do
      liftIO $ log "Listening"
      (connectionSocket, _) <- liftIO $ Network.Socket.accept listeningSocket
      liftIO $ log "Client connected"
      let finalizer = liftIO $ Network.sClose connectionSocket
      forkFinallyCIO finalizer $ do
        let settings = (connectionSocket, timeout, auth, processMessage)
        ei <- liftIO $ runEitherT $ Session.run Session.interact settings
        either (liftIO . log . ("Session error: " <>)) (const $ return ()) ei
  
  return $ void $ killCIO listenerFork


-- | Settings of how to run the server.
type Settings i o s = (ListeningMode, Session.Timeout, Log, Session.ProcessMessage i o s)

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
-- A logging function.
-- If you want no logging, use @('Control.Monad.void' . return)@, 
-- which is a fancy way of alternatively saying @(\\_ -> return ())@.
-- If you want to output to console use @Data.Text.IO.'Data.Text.IO.putStrLn'@.
-- If you want to somehow reformat the output, you're welcome: 
-- @(Data.Text.IO.'Data.Text.IO.putStrLn' . (\"MessagingService.Server: \" `<>`))@.
type Log = Text -> IO ()

-- |
-- A function which shuts the server down, while closing all sockets.
type Stop = CIO ()
