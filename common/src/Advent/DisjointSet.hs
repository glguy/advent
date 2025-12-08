{-# Language BlockArguments #-}
{-|
Module      : Advent.DisjointSet
Description : Implementation of a Disjoint Set datastructure
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.DisjointSet (
   DisjointSet,
   newDisjointSet,
   unifySets,
   setSize,
   setRepresentative,
   inSameSet,
   ) where

import Control.Monad ( when )
import Data.Array.IO (Ix(range), IOArray, newListArray, readArray, writeArray)

newtype DisjointSet a = DS (IOArray a (Int, a))

newDisjointSet :: Ix a => (a, a) -> IO (DisjointSet a)
newDisjointSet b =
 do arr <- newListArray b [(1, x) | x <- range b]
    pure (DS arr)

findRoot' :: Ix a => DisjointSet a -> a -> IO (Int, a)
findRoot' (DS arr) x =
 do (sz, y) <- readArray arr x
    if x == y then pure (sz, x) else findRoot' (DS arr) y

updateRoot :: Ix a => DisjointSet a -> a -> a -> IO ()
updateRoot (DS arr) root x =
  when (root /= x)
   do (sz, y) <- readArray arr x
      writeArray arr x (sz, root)
      updateRoot (DS arr) root y

findRoot :: Ix a => DisjointSet a -> a -> IO (Int, a)
findRoot ds x =
 do (rank, root) <- findRoot' ds x
    updateRoot ds root x
    pure (rank, root)

setRepresentative :: Ix a => DisjointSet a -> a -> IO a
setRepresentative ds x = snd <$> findRoot ds x

setSize :: Ix a => DisjointSet a -> a -> IO Int
setSize ds x =
 do (size, _) <- findRoot ds x
    pure size

-- | Unify two sets into one. Returns True when the unification
-- successfully unified two sets. Returns False when the sets
-- were already unified.
unifySets :: Ix a => DisjointSet a -> a -> a -> IO Bool
unifySets (DS arr) x y =
 do (sizeX, x') <- findRoot (DS arr) x
    (sizeY, y') <- findRoot (DS arr) y
    let success = x' /= y'
    success <$ when success
        if sizeX < sizeY
          then do writeArray arr x' (0, y')
                  writeArray arr y' (sizeX + sizeY, y')
          else do writeArray arr y' (0, x')
                  writeArray arr x' (sizeX + sizeY, x')

inSameSet :: Ix a => DisjointSet a -> a -> a -> IO Bool
inSameSet ds x y =
 do x' <- setRepresentative ds x
    y' <- setRepresentative ds y
    pure (x' == y')
