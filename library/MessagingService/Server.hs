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
  )
  where

import MessagingService.Util.Prelude hiding (listen)
import qualified MessagingService.Server.Session as Session
import qualified MessagingService.Util.FileSystem as FS
import qualified Network
import qualified Network.Socket


-- | 
-- Start the server with the provided settings and
-- return a handle to its thread, which can later be used to stop it,
-- or block until it raises any exception (which really should not be).
-- 
-- Here is an example of how you can run and stop your server: 
-- 
-- @
-- import CIO
-- import qualified MessagingService.Server as Server
-- main = do
--   runCIO 100 $ do
--     let settings = ...
--     -- Run the server asynchronously
--     serverFork <- Server.start settings
--     -- This is how we can stop the server:
--     let stopServer = killCIO serverFork
--     ...
--     -- This is how we can block until the \"serverFork\" thread finishes either
--     -- by means of \"stopServer\" or an exception get thrown.
--     -- In case of exceptions \"waitCIO\" rethrows them in the current thread.
--     waitCIO serverFork
-- @
-- 
-- In the above example we've set the maximum number of concurrent threads to 100.
-- The amount of connections your server handles directly depends on that parameter.
-- All the exceeding connections get put on a wait list.
-- It must also be mentioned that using "killCIO" to stop servers is safe,
-- and all the resources get released.
-- 
-- Why the `CIO` monad? Because composition, that's why!
-- 
-- * You can run multiple servers with a shared thread pool,
-- meaning a shared connection pool. 
-- 
-- * You can compose them with whatever other concurrent things you do, 
-- but still share a thread pool.
-- 
-- * You can kill groups of servers and other concurrent things, e.g.:
-- 
-- @
-- main = do
--   ...
--   -- Everything inside that block shares a pool of a hundred threads.
--   runCIO 100 $ do
--     groupFork <- forkCIO $ do
--       Server.start server1Settings
--       Server.start server2Settings
--       ... -- whatever you like
--     let killThemAll = killCIO groupFork
--     ...
-- @
-- 
start :: (Serializable IO i, Serializable IO o) => Settings i o s -> CIO (ForkCIO ())
start (listeningMode, timeout, log, processMessage) = do

  let (portID, auth) = case listeningMode of
        ListeningMode_Host port auth -> (Network.PortNumber $ fromIntegral port, auth)
        ListeningMode_Socket path -> (Network.UnixSocket $ FS.encodeString path, const $ pure True)

  listeningSocket <- liftIO $ Network.listenOn portID

  let onDeath = liftIO $ Network.sClose listeningSocket
  forkOnDeathCIO onDeath $ do
    forever $ do
      liftIO $ log "Listening"
      (connectionSocket, _) <- liftIO $ Network.Socket.accept listeningSocket
      liftIO $ log "Client connected"
      let onDeath = liftIO $ Network.sClose connectionSocket
      forkOnDeathCIO onDeath $ do
        let settings = (connectionSocket, timeout, auth, processMessage)
        ei <- liftIO $ runEitherT $ Session.run Session.interact settings
        either (liftIO . log . ("Session error: " <>)) (const $ return ()) ei

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
