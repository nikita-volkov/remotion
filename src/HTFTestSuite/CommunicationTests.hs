{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.CommunicationTests where

import Test.Framework
import Remotion.Util.Prelude
import qualified Remotion.Client as Client
import qualified Remotion.Server as Server
import qualified HTFTestSuite.Setup1 as Setup1


test_socketConnection = do
  state <- newMVar 0
  r <- Setup1.runStack (serverSettings state) clientSettings $ do
    lift $ Client.request $ Setup1.Increase
    lift $ Client.request $ Setup1.Increase
    lift $ Client.request $ Setup1.Get
  assertEqual (Right $ Right 2) r
  where
    serverSettings state = (pv, lm, to, mc, lo, pr) where
      pv = 1
      lm = Server.Socket Setup1.socket
      to = 500 * 10 ^ 3
      mc = 100
      lo = Setup1.logToConsole
      pr = Setup1.processRequest state
    clientSettings = (url, upv) where
      url = Client.Socket Setup1.socket
      upv = 1

