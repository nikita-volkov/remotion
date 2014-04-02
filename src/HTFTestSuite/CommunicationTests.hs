{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.CommunicationTests where

import Test.Framework
import Remotion.Util.Prelude hiding (State, state)
import qualified Remotion.Client as C
import qualified Remotion.Server as S


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
hostURL = C.Host "127.0.0.1" port credentials
credentials = Just "p1"
dir = "./dist/test/"
timeout = 500 * 10 ^ 3

runServeT :: 
  (MonadIO m) => 
  (S.UserProtocolVersion, S.ListeningMode, S.Timeout, S.MaxClients) ->
  S.ServeT m a -> 
  m a
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
  assertEqual (Right $ Right 2) =<< do
    runServeT serverSettings $ runConnectionT clientSettings $ do
      C.request $ Increase
      C.request $ Increase
      C.request $ Get
  where
    serverSettings = (1, socketLM, timeout, 100)
    clientSettings = (1, socketURL)

test_serverResourcesGetReleased = do
  runServeT serverSettings (return ())
  runServeT serverSettings (return ())
  return () :: IO ()
  where
    serverSettings = (1, socketLM, timeout, 100)

test_hostConnection = do
  assertEqual (Right $ Right 2) =<< do
    runServeT serverSettings $ runConnectionT clientSettings $ do
      C.request $ Increase
      C.request $ Increase
      C.request $ Get
  where
    serverSettings = (1, hostLM, timeout, 100)
    clientSettings = (1, hostURL)


