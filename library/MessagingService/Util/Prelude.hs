module MessagingService.Util.Prelude 
  ( 
    module Exports,

    LazyByteString,
    LazyText,

    (?:),
    traceM,
    applyAll,
    packText,
    unpackText,
    bug,
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
import Data.Data as Exports
import Text.Read as Exports (readMaybe, readEither)
import Control.Exception as Exports hiding (tryJust)
import Control.Concurrent as Exports hiding (yield)
import System.Mem.StableName as Exports
import System.Timeout as Exports
import System.Exit as Exports
import System.IO.Unsafe as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import Unsafe.Coerce as Exports
import GHC.Exts as Exports hiding (traceEvent)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import Debug.Trace as Exports
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

-- errors
import Control.Error as Exports

-- placeholders
import Development.Placeholders as Exports

-- bytestring
import Data.ByteString as Exports (ByteString)

-------------
-- Data
-------------

-- time
import Data.Time.Clock as Exports

-- text
import Data.Text as Exports (Text)

-- containers
import Data.Map as Exports (Map)
import Data.IntMap as Exports (IntMap)
import Data.Set as Exports (Set)
import Data.IntSet as Exports (IntSet)
import Data.Sequence as Exports (Seq)
import Data.Tree as Exports (Tree)

-- system-filepath
import Filesystem.Path as Exports (FilePath)

-- hashable
import Data.Hashable as Exports (Hashable(..), hash)

-------------
-- Streaming
-------------

-- pipes
import Pipes as Exports

-- pipes-cereal-plus
import PipesCerealPlus as Exports

-------------
-- Concurrency
-------------

-- stm
import Control.Concurrent.STM as Exports

-- cio
import CIO as Exports


import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Data.Text
import qualified Prelude


type LazyByteString = Data.ByteString.Lazy.ByteString
type LazyText = Data.Text.Lazy.Text


(?:) :: Maybe a -> a -> a
maybeA ?: b = fromMaybe b maybeA
{-# INLINE (?:) #-}

traceM :: (Monad m) => String -> m ()
traceM s = trace s $ return ()

applyAll :: Monad m => [a -> m b] -> a -> m [b]
applyAll ops a = sequence $ map ($ a) ops

packText = Data.Text.pack
unpackText = Data.Text.unpack

bug = placeholderNoWarning . (++) "'messaging-service' package bug: "
