module MessagingService.Server where

import MessagingService.Util.Prelude
import qualified MessagingService.Protocol as Protocol
import qualified MessagingService.Server.Session as Session
import qualified MessagingService.Util.FileSystem as FS
import qualified Network
import qualified Network.Socket
import qualified Pipes.Network.TCP.Safe as PipesNetwork
import qualified Pipes.Prelude as PipesPrelude


-- | 
-- Start the server with the provided settings.
-- 
-- This is a blocking operation.
-- If all your executable does is just run the server, 
-- then all you need is just to run the `CIO` monad,
-- setting the maximum number of concurrent threads, 
-- which controls the amount of maintained connections.
-- All the exceeding connections will be put on a wait list.
-- In the following case we set it to 100:
-- 
-- @
-- main = runCIO 100 $ listen settings
--   where settings = ...
-- @
-- 
-- If your executable does more than just run the server,
-- you should run the server asynchronously. 
-- This will also provide you with the ability to stop it.
-- E.g.:
-- This is all achievable with the standard concurrency primitives of the "cio" library.
-- 
-- @
-- main = do
--   runCIO 100 $ do
--     let settings = ...
--     -- Run the server asynchronously
--     serverFork <- forkCIO $ listen settings
--     -- This is how we can stop the server:
--     let stopServer = killCIO serverFork
--     ...
--     -- This is how we can block until the `serverFork` thread finishes either
--     -- by means of `stopServer` or an exception get thrown.
--     -- In case of exceptions `waitCIO` rethrows them in the current thread.
--     waitCIO serverFork
-- @
-- 
-- Why the `CIO` monad? Because composition!!
-- 
-- * You can run multiple servers with a shared thread pool,
-- meaning a shared connection pool. 
-- 
-- * You can compose it with whatever other concurrent things you do, 
-- but still share a thread pool.
-- 
-- * You can kill groups of servers and other concurrent things, e.g.:
-- 
-- @
-- main = do
--   ...
--   -- Everything inside that block shares a pool of a hundred threads.
--   runCIO 100 $ do
--     groupFork <- 
--       forkCIO $ do
--         forkCIO $ listen server1Settings
--         forkCIO $ listen server2Settings
--         forkCIO $ ... -- whatever you like
--     let killThemAll = killCIO groupFork
--     ...
-- @
-- 
listen :: (Serializable IO i, Serializable IO o) => Settings i o s -> CIO ()
listen (listeningMode, timeout, log, processMessage) = do

  listeningSocket <- do
    let portID = case listeningMode of
          ListeningMode_Host port _ -> Network.PortNumber $ fromIntegral port
          ListeningMode_Socket path -> Network.UnixSocket $ FS.encodeString path
    liftIO $ Network.listenOn portID

  forever $ do
    liftIO $ log "Listening"
    (connectionSocket, _) <- liftIO $ Network.Socket.accept listeningSocket
    liftIO $ log "Client connected"
    forkCIO $ runSession connectionSocket

  return ()
  where
    -- If things get trickier, we should make it a dedicated monad.
    runSession socket = do
      $notImplemented
      where
        listen :: forall a. (Serializable IO a) => IO (Either Text a)
        listen = 
          fmap join $ (fmap . fmap) (maybe (Left "Empty request") Right) $
          runEitherT $ PipesPrelude.head $ 
            PipesNetwork.fromSocketTimeout timeout socket 4096 >-> 
            deserializingPipe
        reply :: forall a. (Serializable IO a) => a -> IO ()
        reply a = runEffect $ 
          serializingProducer a >-> 
          PipesNetwork.toSocketTimeout timeout socket

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

