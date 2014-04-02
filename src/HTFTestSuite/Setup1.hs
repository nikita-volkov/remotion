{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HTFTestSuite.Setup1 where

import Test.Framework
import Remotion.Util.Prelude hiding (State, state)
import qualified Remotion.Client as Client
import qualified Remotion.Server as Server
import qualified Data.Text.IO


type Communicate m r = 
  Client.ConnectionT Request Response (Server.ServeT m) r

data Request = 
  Increase | Decrease | Multiply Float | Divide Float | Get
  deriving (Show, Ord, Eq, Generic)

instance Serializable m Request

type Response = Either () Float

type State = MVar Float

processRequest :: State -> Server.State s -> Request -> IO Response
processRequest state clientState = \case
  Increase -> modifyMVar_ state (pure . succ) >> return (Left ())
  Decrease -> modifyMVar_ state (pure . pred) >> return (Left ())
  Multiply by -> modifyMVar_ state (pure . (*by)) >> return (Left ())
  Divide by -> modifyMVar_ state (pure . (/by)) >> return (Left ())
  Get -> readMVar state |$> Right

logToConsole = Data.Text.IO.putStrLn
dontLog = void . return

socket = dir <> ".socket"
port = 45039
credentials = "p1"
dir = "./dist/test/"
timeout = 500 * 10 ^ 3

runStack :: 
  Server.Settings Request Response s -> Client.Settings -> Communicate IO r -> 
  IO (Either Client.Failure r)
runStack s c m = 
  Client.runConnectionT c m |> Server.runServeT s
