module MessagingService.Server where

import MessagingService.Util.Prelude
import qualified MessagingService.Protocol as Protocol
import qualified MessagingService.Server.ClientState as ClientState
import qualified MessagingService.Util.FileSystem as FS

import qualified Network
import qualified Network.Socket
import qualified Network.Simple.TCP as NetworkSimple
import qualified Pipes.Network.TCP.Safe as PipesNetwork
import qualified Pipes.Prelude as PipesPrelude
import qualified Data.Set as Set


-- | Start the server with the provided settings.
start :: (Serializable IO i, Serializable IO o) => Settings i o s -> IO (Server i o s)
start (listeningMode, timeout, log, processMessage) = do
  listeningSocket <- do
    let portID = case listeningMode of
          ListeningMode_Host port _ -> Network.PortNumber $ fromIntegral port
          ListeningMode_Socket path -> Network.UnixSocket $ FS.encodeString path
    Network.listenOn portID
  $notImplemented

-- | Settings of how to run the server.
type Settings i o s = (ListeningMode, Timeout, Log, ProcessMessage i o s)

-- | Defines how to listen for connections.
data ListeningMode =
  -- | 
  -- Listen on a port with an authentication function.
  ListeningMode_Host Port Authenticate |
  -- | 
  -- Listen on a socket file.
  -- Since sockets are local no authentication is needed.
  -- Works only on UNIX systems.
  ListeningMode_Socket FilePath

-- | A port to run the server on.
type Port = Int

-- | 
-- A function, which checks the hashed authentication data.
-- If you want to provide access to anybody, use @(\_ -> return True)@.
-- 
-- Hashes can be either plain ASCII passwords or an encoding of some data, 
-- e.g. an MD5 hash of a login-password pair or just a password.
-- 
-- An argument value of @Nothing@ means an attempt of anonymous authentication.
type Authenticate = Maybe ByteString -> IO Bool

-- |
-- A session timeout in ms. Period of keepalive signaling depends on that parameter.
-- If you don't want excessive requests, just make it a couple of minutes.
type Timeout = Int

-- |
-- A logging function.
-- If you want no logging, use @('Control.Monad.void' . return)@, 
-- which is a fancy way of alternatively saying @(\\_ -> return ())@.
-- If you want to output to console use @Data.Text.IO.'Data.Text.IO.putStrLn'@.
-- If you want to somehow reformat the output, you're welcome: 
-- @(Data.Text.IO.'Data.Text.IO.putStrLn' . (\"MessagingService.Server: \" `<>`))@.
type Log = Text -> IO ()

-- | 
-- A function which processes messages from client and produces a response,
-- while managing a user-defined session state for each client.
-- Since we're in `IO` to have a mutable state you can use mutable data structures and `IORef`s.
-- 
-- This function essentially is what defines what your server actually does.
type ProcessMessage i o s = s -> i -> IO o

-- |
-- A messaging server state.
data Server i o s = Server {
  stop :: IO ()
}



