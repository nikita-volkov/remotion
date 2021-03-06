module HTFTestSuite.Prelude 
  ( 
    module Exports,

    LazyByteString,
    LazyText,

    (?:),
    traceM,
    traceIO,
    traceIOWithTime,
    packText,
    unpackText,
    bug,
    (|>),
    (<|),
    (|$>),
    microsToDiff,
    diffToMicros,
    bracketME,
    finallyME,
    tracingExceptions,
  )
  where

-------------
-- Control
-------------

-- base
import Prelude as Exports hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, FilePath, id, (.))
import Control.Monad as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Applicative as Exports
import Control.Arrow as Exports hiding (left, right)
import Control.Category as Exports
import Data.Monoid as Exports hiding (Any)
import Data.Foldable as Exports
import Data.Traversable as Exports hiding (for)
import Data.Maybe as Exports
import Data.List as Exports hiding (concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
import Data.Tuple as Exports
import Data.Ord as Exports (Down(..))
import Data.String as Exports
import Data.Int as Exports
import Data.Word as Exports
import Data.Ratio as Exports
import Data.Fixed as Exports
import Data.Ix as Exports
import Data.Data as Exports hiding (Proxy)
import Text.Read as Exports (readMaybe, readEither)
import Control.Exception as Exports hiding (tryJust, assert)
import System.Mem.StableName as Exports
import System.Exit as Exports
import System.IO.Unsafe as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import Unsafe.Coerce as Exports
import GHC.Exts as Exports hiding (traceEvent, toList)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import Debug.Trace as Exports hiding (traceIO, traceM)
import Data.IORef as Exports
import Data.STRef as Exports
import Control.Monad.ST as Exports

-- mtl
import Control.Monad.Identity as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.State as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.Reader as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.Writer as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM, Any)
import Control.Monad.Trans as Exports
import Control.Monad.Error as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)

-- transformers-base
import Control.Monad.Base as Exports

-- monad-control
import Control.Monad.Trans.Control as Exports

-- errors
import Control.Error as Exports hiding ((?:))

-- placeholders
import Development.Placeholders as Exports

-- loch-th
import Debug.Trace.LocationTH as Exports

-------------
-- Data
-------------

-- time
import Data.Time.Clock as Exports

-- bytestring
import Data.ByteString as Exports (ByteString)

-- text
import Data.Text as Exports (Text)

-- containers
import Data.Map as Exports (Map)
import Data.IntMap as Exports (IntMap)
import Data.Set as Exports (Set)
import Data.IntSet as Exports (IntSet)
import Data.Sequence as Exports (Seq)
import Data.Tree as Exports (Tree)

-- hashable
import Data.Hashable as Exports (Hashable(..), hash)

-------------
-- Concurrency
-------------

-- base
import Control.Concurrent as Exports hiding (yield)

-- stm
import Control.Concurrent.STM as Exports hiding (check)

-------------
-- File-system
-------------

-- system-filepath
import Filesystem.Path as Exports (FilePath)

-------------
-- Streaming
-------------

-- pipes
import Pipes as Exports

-- pipes-cereal-plus
import PipesCerealPlus as Exports


import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Data.Text
import qualified Prelude
import qualified Debug.Trace
import qualified System.Locale
import qualified Data.Time


type LazyByteString = Data.ByteString.Lazy.ByteString
type LazyText = Data.Text.Lazy.Text


(?:) :: Maybe a -> a -> a
maybeA ?: b = fromMaybe b maybeA
{-# INLINE (?:) #-}

traceM :: (Monad m) => String -> m ()
traceM s = trace s $ return ()

traceIO :: (MonadIO m) => String -> m ()
traceIO = liftIO . Debug.Trace.traceIO

traceIOWithTime :: (MonadIO m) => String -> m ()
traceIOWithTime s = do
  time <- liftIO $ getCurrentTime
  traceIO $ 
    formatTime time <> ": " <> s
  where
    formatTime = 
      take 15 . 
      Data.Time.formatTime System.Locale.defaultTimeLocale "%X.%q"

packText = Data.Text.pack
unpackText = Data.Text.unpack

bug = [e| $failure . (msg <>) . Data.Text.unpack |]
  where
    msg = "A \"remotion\" package bug: " :: String

(|>) :: a -> (a -> b) -> b
a |> aToB = aToB a
{-# INLINE (|>) #-}

(<|) :: (a -> b) -> a -> b
aToB <| a = aToB a
{-# INLINE (<|) #-}

-- | 
-- The following are all the same:
-- fmap f a == f <$> a == a |> fmap f == a |$> f
-- 
-- This operator accomodates the left-to-right operators: >>=, >>>, |>.
(|$>) = flip fmap
{-# INLINE (|$>) #-}

microsToDiff :: Fractional c => Integer -> c
microsToDiff = fromRational . (%(10^6))

diffToMicros :: (Real a, Integral b) => a -> b
diffToMicros = round . (*(10^6)) . toRational

bracketME :: (MonadError e m) => m a -> (a -> m b) -> (a -> m c) -> m c
bracketME acquire release apply = do
  r <- acquire
  z <- catchError (liftM Right $ apply r) (return . Left)
  release r
  either throwError return z

finallyME :: (MonadError e m) => m a -> m b -> m a
finallyME m f = do
  z <- catchError (liftM Right $ m) (return . Left)
  f
  either throwError return z

tracingExceptions :: MonadBaseControl IO m => m a -> m a
tracingExceptions m = 
  control $ \runInIO -> catch (runInIO m) $ \(SomeException e) -> runInIO $ do
    let rep = typeOf e
        tyCon = typeRepTyCon rep
    traceM $ 
      "## Uncaught exception: " ++ show e ++ "\n" ++
      "   Type: " ++ show rep ++ "\n" ++
      "   Module: " ++ tyConModule tyCon ++ "\n" ++
      "   Package: " ++ tyConPackage tyCon
    liftBase $ throwIO $ e
