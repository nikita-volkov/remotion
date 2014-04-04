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

logToConsole = traceIOWithTime . ("Server: " <>) . unpackText
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
  traceIO "--- \"socketConnection\" log ---"
  forM_ [1..20] $ \n -> do
    traceIO $ "--- Attempt " <> show n <> " ---"
    assertEqual (Right $ Right $ Right 2) =<< do
      runServeT serverSettings $ runConnectionT clientSettings $ do
        C.request $ Increase
        C.request $ Increase
        C.request $ Get
  traceIO "---"
  where
    serverSettings = (1, socketLM, timeout, 100)
    clientSettings = (1, socketURL)

test_hostConnection = do
  traceIO "--- \"hostConnection\" log ---"
  forM_ [1..20] $ \n -> do
    traceIO $ "--- Attempt " <> show n <> " ---"
    assertEqual (Right $ Right $ Right 2) =<< do
      runServeT serverSettings $ do
        traceIOWithTime $ "Running connection"
        r <- runConnectionT clientSettings $ do
          traceIOWithTime $ "Requesting"
          C.request $ Increase
          C.request $ Increase
          r <- C.request $ Get
          traceIOWithTime $ "Finishing connection"
          return r
        traceIOWithTime $ "Finishing server"
        return r
  traceIO "---"
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
  traceIO "--- \"requestAnOfflineServer\" log ---"
  forM_ [1..10] $ \n -> do
    traceIO $ "--- Attempt " <> show n <> " ---"
    assertEqual (Left $ C.ConnectionInterrupted) =<< do
      forkIO $ do
        runServeT serverSettings $ do
          liftIO $ threadDelay $ 3*timeUnit
        return ()
      liftIO $ threadDelay $ 1*timeUnit
      runConnectionT clientSettings $ do
        liftIO $ threadDelay $ 3*timeUnit
        C.request Increase
  traceIO "---"
  where
    timeUnit = 10^5
    timeout = 3*timeUnit
    serverSettings = (1, hostLM, timeout, 100)
    clientSettings = (1, hostURL)

test_invalidClientRequests = unitTestPending ""

test_clientConnectAcquiresASlot = do
  runServeT serverSettings $ do
    slots <- S.countSlots
    r <- runConnectionT clientSettings $ lift $ S.countSlots
    liftIO $ assertEqual (Right $ pred slots) r
  return () :: IO ()

test_clientDisconnectReleasesASlot = do
  runServeT serverSettings $ do
    slots <- S.countSlots
    runConnectionT clientSettings $ return ()
    slots' <- S.countSlots
    liftIO $ assertEqual slots slots'
  return () :: IO ()

test_keepalive = do
  runServeT serverSettings $ do
    slots <- S.countSlots
    As.async $ runConnectionT clientSettings $ liftIO $ threadDelay $ timeUnit * 3
    liftIO $ threadDelay $ timeUnit * 2
    slots' <- S.countSlots
    liftIO $ assertEqual (slots - 1) slots'
  return () :: IO ()
  where
    timeUnit = 10^5
    timeout = timeUnit * 1
    serverSettings = (1, hostLM, timeout, 100)
    clientSettings = (1, hostURL)

test_multipleClients = do
  runServeT serverSettings $ do
    updates <- do
      As.mapConcurrently id $ replicate 9 $ 
        runConnectionT clientSettings $ C.request Increase
    state <- runConnectionT clientSettings $ C.request Get
    liftIO $ do
      assertEqual (replicate 9 $ Right $ Left ()) updates
      assertEqual (Right $ Right $ 9) state
  return () :: IO ()

test_highLoad = do
  runServeT serverSettings $ do
    As.mapConcurrently id $ replicate 100 $ do
      runConnectionT clientSettings $ do
        replicateM 100 $ C.request Increase
    r <- runConnectionT clientSettings $ C.request Get
    liftIO $ assertEqual (Right $ Right $ 100 * 100) $ r
  return () :: IO ()
  where
    serverSettings = (1, hostLM, timeout, 110)
    clientSettings = (1, hostURL)

