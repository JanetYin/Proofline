module Metacontext where

import Data.IORef
import System.IO.Unsafe

import qualified Data.IntMap as IM

import Common
import Value

--------------------------------------------------------------------------------

data MetaEntry = Solved Value | Unsolved { 
  metaContext :: [(Name, VTy)],
  expectedType :: VTy 
}

nextMeta :: IORef Int
nextMeta = unsafeDupablePerformIO $ newIORef 0    -- explaination about why we use this
{-# noinline nextMeta #-}

mcxt :: IORef (IM.IntMap MetaEntry)               -- how to understand this? in "production", we'd have mutable array instead of IntMap
mcxt = unsafeDupablePerformIO $ newIORef mempty
{-# noinline mcxt #-}

lookupMeta :: MetaVar -> MetaEntry
lookupMeta (MetaVar m) = unsafeDupablePerformIO $ do
  ms <- readIORef mcxt
  case IM.lookup m ms of
    Just e  -> pure e
    Nothing -> error ("impossible meta: " ++ show m)

reset :: IO ()
reset = do
  writeIORef nextMeta 0
  writeIORef mcxt mempty
  
getMetas :: IO [(Int, MetaEntry)]
getMetas = do
  ms <- readIORef mcxt
  return $ IM.toList ms