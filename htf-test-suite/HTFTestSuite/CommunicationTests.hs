{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.CommunicationTests where

import Test.Framework
import HTFTestSuite.Prelude hiding (traceIO, traceIOWithTime, State, state)
import qualified HTFTestSuite.Prelude as Prelude
import qualified Remotion.Client as C
import qualified Remotion.Server as S
import qualified Control.Concurrent.Async.Lifted as As


-- Debugging
-------------------------
-- The following functions get enabled during debugging.

debugging = False
prefix = id
traceIO = if debugging 
  then Prelude.traceIO . prefix 
  else const $ return ()
traceIOWithTime = if debugging 
  then Prelude.traceIOWithTime . prefix 
  else const $ return ()

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
serverSettings = ("1", hostLM, timeout, 100)
clientSettings = ("1", hostURL)

runServer :: 
  (MonadIO m) => 
  (S.UserProtocolSignature, S.ListeningMode, S.Timeout, S.MaxClients) ->
  S.Server m a -> 
  m (Either S.Failure a)
runServer (upv, lm, to, mc) t = do
  state <- liftIO $ newMVar 0
  S.run (settings state) t
  where
    settings state = (upv, lm, to, mc, logToConsole, processRequest state)

runClient :: 
  (MonadBaseControl IO m, MonadIO m) =>
  C.Settings -> C.Client Request Response m a -> m (Either C.Failure a)
runClient = C.run


-- Tests
-------------------------

test_socketConnection = do
  traceIO "--- \"socketConnection\" log ---"
  forM_ [1..20] $ \n -> do
    traceIO $ "--- Attempt " <> show n <> " ---"
    assertEqual (Right $ Right $ Right 2) =<< do
      runServer serverSettings $ runClient clientSettings $ do
        C.request $ Increase
        C.request $ Increase
        C.request $ Get
  traceIO "---"
  where
    serverSettings = ("1", socketLM, timeout, 100)
    clientSettings = ("1", socketURL)

test_hostConnection = do
  traceIO "--- \"hostConnection\" log ---"
  forM_ [1..20] $ \n -> do
    traceIO $ "--- Attempt " <> show n <> " ---"
    assertEqual (Right $ Right $ Right 2) =<< do
      runServer serverSettings $ do
        traceIOWithTime $ "Running connection"
        r <- runClient clientSettings $ do
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
    serverSettings = ("1", hostLM, timeout, 100)
    clientSettings = ("1", hostURL)

test_serverResourcesGetReleased = do
  assertEqual (Right ()) =<< do
    runServer serverSettings (return ())
    runServer serverSettings (return ())
  where
    serverSettings = ("1", socketLM, timeout, 100)

test_invalidCredentials = do
  assertEqual (Right $ Left $ C.Unauthenticated) =<< do
    runServer serverSettings $ runClient clientSettings $ return ()
  where
    serverSettings = ("1", hostLM, timeout, 100)
    clientSettings = ("1", C.Host host port (Just "1"))

test_noCredentials = do
  assertEqual (Right $ Left $ C.Unauthenticated) =<< do
    runServer serverSettings $ runClient clientSettings $ return ()
  where
    serverSettings = ("1", hostLM, timeout, 100)
    clientSettings = ("1", C.Host host port Nothing)

test_connectingToAnOfflineServer = do
  assertEqual (Left $ C.UnreachableURL) =<< do
    runServer serverSettings (return ())
    runClient clientSettings $ return ()
  where
    serverSettings = ("1", hostLM, timeout, 100)
    clientSettings = ("1", hostURL)

test_unmatchingUserProtocolSignatures = do
  assertEqual (Right $ Left $ C.UserProtocolSignatureMismatch "2" "1") =<< do
    runServer serverSettings $ runClient clientSettings $ return ()
  where
    serverSettings = ("1", hostLM, timeout, 100)
    clientSettings = ("2", hostURL)

test_requestAnOfflineServer = do
  traceIO "--- \"requestAnOfflineServer\" log ---"
  forM_ [1..10] $ \n -> do
    traceIO $ "--- Attempt " <> show n <> " ---"
    assertEqual (Left $ C.ConnectionInterrupted) =<< do
      forkIO $ do
        runServer serverSettings $ do
          liftIO $ threadDelay $ 3*timeUnit
        return ()
      liftIO $ threadDelay $ 1*timeUnit
      runClient clientSettings $ do
        liftIO $ threadDelay $ 3*timeUnit
        C.request Increase
  traceIO "---"
  where
    timeUnit = 10^5
    timeout = 3*timeUnit
    serverSettings = ("1", hostLM, timeout, 100)
    clientSettings = ("1", hostURL)

test_clientConnectAcquiresASlot = do
  runServer serverSettings $ do
    slots <- S.countSlots
    r <- runClient clientSettings $ lift $ S.countSlots
    liftIO $ assertEqual (Right $ pred slots) r
  return () :: IO ()

test_clientDisconnectReleasesASlot = do
  runServer serverSettings $ do
    slots <- S.countSlots
    runClient clientSettings $ return ()
    slots' <- S.countSlots
    liftIO $ assertEqual slots slots'
  return () :: IO ()

test_keepalive = do
  runServer serverSettings $ do
    slots <- S.countSlots
    As.async $ runClient clientSettings $ liftIO $ threadDelay $ timeUnit * 3
    liftIO $ threadDelay $ timeUnit * 2
    slots' <- S.countSlots
    liftIO $ assertEqual (slots - 1) slots'
  return () :: IO ()
  where
    timeUnit = 10^5
    timeout = timeUnit * 1
    serverSettings = ("1", hostLM, timeout, 100)
    clientSettings = ("1", hostURL)

test_multipleClients = do
  runServer serverSettings $ do
    updates <- do
      As.mapConcurrently id $ replicate 9 $ 
        runClient clientSettings $ C.request Increase
    state <- runClient clientSettings $ C.request Get
    liftIO $ do
      assertEqual (replicate 9 $ Right $ Left ()) updates
      assertEqual (Right $ Right $ 9) state
  return () :: IO ()

test_highLoad = do
  runServer serverSettings $ do
    As.mapConcurrently id $ replicate 100 $ do
      runClient clientSettings $ do
        replicateM 100 $ C.request Increase
    r <- runClient clientSettings $ C.request Get
    liftIO $ assertEqual (Right $ Right $ 100 * 100) $ r
  return () :: IO ()
  where
    serverSettings = ("1", hostLM, timeout, 110)
    clientSettings = ("1", hostURL)

test_concurrentRequestsFromASingleClient = do
  assertEqual (Right $ Right $ Right $ 1000) =<< do
    runServer serverSettings $ runClient clientSettings $ do
      void $ As.mapConcurrently id $ replicate 100 $ do
        replicateM_ 10 $ do
          C.request Increase
          C.request Increase
          C.request Decrease
      C.request Get 

test_tooManyConnections = do
  assertEqual (Right $ Left $ C.ServerIsBusy) =<< do
    runServer serverSettings $ do
      a <- As.async $ runClient clientSettings $ liftIO $ threadDelay $ 10^3*100
      b <- As.async $ runClient clientSettings $ liftIO $ threadDelay $ 10^3*100
      liftIO $ threadDelay $ 10^3*50
      r <- runClient clientSettings $ return ()
      mapM_ As.wait [a, b]
      return r
  where
    serverSettings = ("1", hostLM, timeout, 2)
    clientSettings = ("1", hostURL)

test_lastConnectionSlot = do
  assertEqual (Right $ Right $ ()) =<< do
    runServer serverSettings $ do
      a <- As.async $ runClient clientSettings $ liftIO $ threadDelay $ 10^3*100
      liftIO $ threadDelay $ 10^3*50
      (runClient clientSettings $ return ()) <* As.wait a
  where
    serverSettings = ("1", hostLM, timeout, 2)
    clientSettings = ("1", hostURL)

test_multipleHittersOnTooManyConnectionsStillGetSurved = do
  assertEqual (Right $ replicate 10 $ Left $ C.ServerIsBusy) =<< do
    runServer serverSettings $ do
      As.withAsync (runClient clientSettings $ liftIO $ threadDelay $ 10^3*100) $ \_ -> 
        As.withAsync (runClient clientSettings $ liftIO $ threadDelay $ 10^3*100) $ \_ -> do
          liftIO $ threadDelay $ 10^3*50
          As.mapConcurrently id $ replicate 10 $ runClient clientSettings $ return ()
  where
    serverSettings = ("1", hostLM, timeout, 2)
    clientSettings = ("1", hostURL)

test_invalidClientRequests = do
  state <- newMVar 0
  assertEqual (Right $ Left $ C.CorruptRequest "Out of range") =<< do
    S.run ("1", hostLM, timeout, 10, logToConsole, processRequest state) $ do
      C.run clientSettings $ do
        C.request 'a' :: C.Client Char Int (S.Server IO) Int

