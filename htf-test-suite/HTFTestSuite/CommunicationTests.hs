{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.CommunicationTests where

import Test.Framework
import HTFTestSuite.Prelude hiding (State, state)
import qualified Remotion.Client as C
import qualified Remotion.Server as S
import qualified Control.Concurrent.Async.Lifted as As


-- Setup
-------------------------

data Request = 
  Increase | Decrease | Multiply Float | Divide Float | Get
  deriving (Show, Ord, Eq, Generic)

instance Serializable m Request

type Response = Either () Float

type State = MVar Float

processRequest :: State -> S.State s -> Request -> IO Response
processRequest state clientState = \case
  Increase -> modifyMVar_ state (pure . succ) >> return (Left ())
  Decrease -> modifyMVar_ state (pure . pred) >> return (Left ())
  Multiply by -> modifyMVar_ state (pure . (*by)) >> return (Left ())
  Divide by -> modifyMVar_ state (pure . (/by)) >> return (Left ())
  Get -> readMVar state |$> Right

logToConsole = traceM . unpackText . ("Server: " <>)
dontLog = void . return
socket = dir <> ".socket"
socketURL = C.Socket socket
socketLM = S.Socket socket
port = 45039
hostLM = S.Host port authenticate
authenticate = return . (== credentials) 
hostURL = C.Host host port credentials
host = "127.0.0.1"
credentials = Just "p1"
dir = "./dist/test/"
timeout = 500 * 10 ^ 3
serverSettings = (1, hostLM, timeout, 100)
clientSettings = (1, hostURL)

runServeT :: 
  (MonadIO m) => 
  (S.UserProtocolVersion, S.ListeningMode, S.Timeout, S.MaxClients) ->
  S.ServeT m a -> 
  m (Either S.Failure a)
runServeT (upv, lm, to, mc) t = do
  state <- liftIO $ newMVar 0
  S.runServeT (settings state) t
  where
    settings state = (upv, lm, to, mc, logToConsole, processRequest state)

runConnectionT :: 
  (MonadBaseControl IO m, MonadIO m) =>
  C.Settings -> C.ConnectionT Request Response m a -> m (Either C.Failure a)
runConnectionT = C.runConnectionT


-- Tests
-------------------------

test_socketConnection = do
  assertEqual (Right $ Right $ Right 2) =<< do
    runServeT serverSettings $ runConnectionT clientSettings $ do
      C.request $ Increase
      C.request $ Increase
      C.request $ Get
  where
    serverSettings = (1, socketLM, timeout, 100)
    clientSettings = (1, socketURL)

test_hostConnection = do
  assertEqual (Right $ Right $ Right 2) =<< do
    runServeT serverSettings $ runConnectionT clientSettings $ do
      C.request $ Increase
      C.request $ Increase
      C.request $ Get
  where
    serverSettings = (1, hostLM, timeout, 100)
    clientSettings = (1, hostURL)

test_serverResourcesGetReleased = do
  assertEqual (Right ()) =<< do
    runServeT serverSettings (return ())
    runServeT serverSettings (return ())
  where
    serverSettings = (1, socketLM, timeout, 100)

test_tooManyConnections = unitTestPending "MaxClients setting"

test_invalidCredentials = do
  assertEqual (Right $ Left $ C.Unauthenticated) =<< do
    runServeT serverSettings $ runConnectionT clientSettings $ return ()
  where
    serverSettings = (1, hostLM, timeout, 100)
    clientSettings = (1, C.Host host port (Just "1"))

test_noCredentials = do
  assertEqual (Right $ Left $ C.Unauthenticated) =<< do
    runServeT serverSettings $ runConnectionT clientSettings $ return ()
  where
    serverSettings = (1, hostLM, timeout, 100)
    clientSettings = (1, C.Host host port Nothing)

test_connectingToAnOfflineServer = do
  assertEqual (Left $ C.UnreachableURL) =<< do
    runServeT serverSettings (return ())
    runConnectionT clientSettings $ return ()
  where
    serverSettings = (1, hostLM, timeout, 100)
    clientSettings = (1, hostURL)

test_unmatchingUserProtocolVersions = do
  assertEqual (Right $ Left $ C.UserProtocolVersionMismatch 2 1) =<< do
    runServeT serverSettings $ runConnectionT clientSettings $ return ()
  where
    serverSettings = (1, hostLM, timeout, 100)
    clientSettings = (2, hostURL)

test_requestAnOfflineServer = do
  assertEqual (Left $ C.ConnectionInterrupted) =<< do
    forkIO $ do
      runServeT serverSettings $ do
        liftIO $ threadDelay $ 10^3*500
      return ()
    liftIO $ threadDelay $ 10^3*100
    runConnectionT clientSettings $ do
      liftIO $ threadDelay $ 10^3*500
      C.request Increase
