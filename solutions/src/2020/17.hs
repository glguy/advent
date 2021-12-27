{-# Language ImportQualifiedPost, BlockArguments #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/17>

This automata implementation uses a few optimizations:

It precomputes the neighborhood map that can be quickly
translated to any position without recomputing the map.
Unioning one of these for each live cell gives us a Map
where each element tells us how many neighbors that
cell has. Taking the union of these maps is faster than
inserting individual neighbor elements.

This solution only needs to consider elements that have
any neighbors at all, and these values are determined
by only considering the live elements from the previous
generation.

The solution avoids checking if a particular cell in the
previous generation was alive or not unless it's neighbors
is known to be @2@, as this is the only time it matters.

This solution works both with a flexible, n-dimensional
list coordinate representation and also with more
efficient unpacked tuples of integers. The list version
is about 2x slower than the unpacked tuples.

On my MacBook Pro, part 2 of this problem runs in 50ms.

-}
module Main (main) where

import Advent
import Control.Monad (replicateM)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Find the coordinates of live cells.
--
-- >>> parse [".#.", "..#", "###"]
-- [(1,0),(2,1),(0,2),(1,2),(2,2)]
parse :: [String] -> [(Int,Int)]
parse input = [(x,y) | (y,line) <- zip [0..] input, (x,'#') <- zip [0..] line]

-- |
-- >>> :main
-- 257
-- 2532
main :: IO ()
main =
  do inp <- parse <$> getInputLines 17
     print (run neighborCount3 (map toC3 inp))
     print (run neighborCount4 (map toC4 inp))

run ::
  Ord a =>
  (a -> Map a Int) {- ^ neighbor generator             -} ->
  [a]              {- ^ input coordinates              -} ->
  Int              {- ^ live cells after 6 generations -}
run neighbor = Set.size . times 6 (step neighbor) . Set.fromList

-- | Determine if a cell should be alive in the next generation.
rule ::
  Ord a =>
  Set a {- ^ previous generation      -} ->
  a     {- ^ coordinate               -} ->
  Int   {- ^ live neighbor count      -} ->
  Bool  {- ^ alive in next generation -}
rule world c n = n == 3 || n == 2 && Set.member c world

-- | Compute the next generation from the previous generation
step :: Ord a => (a -> Map a Int) -> Set a -> Set a
step neighbor world
  = Map.keysSet
  $ Map.filterWithKey (rule world)
  $ Map.unionsWith (+)
  $ map neighbor
  $ Set.toList world

-- List-based coordinates ----------------------------------------------

-- Given a dimension and a coordinate this produces a map of all the
-- neighboring cells with values of @1@.
--
-- neighborCountN :: Int -> [Int] -> Map [Int] Int
-- neighborCountN d =
--   let ns = Map.fromList [(c,1) | c <- tail (replicateM d [0,-1,1])]
--   in \c -> Map.mapKeysMonotonic (zipWith (+) c) ns

-- Unpacked 3-tuples ---------------------------------------------------

data C3 = C3 !Int !Int !Int      deriving (Eq, Ord)

toC3 :: (Int,Int) -> C3
toC3 (x,y) = C3 x y 0

-- | Compute a Map with @1@ stored at each neighboring coordinate
neighborCount3 :: C3 -> Map C3 Int
neighborCount3 =
  let ns = Map.fromList [(C3 x y z,1) | [x,y,z] <- tail (replicateM 3 [0,-1,1])]
  in \(C3 a b c) -> Map.mapKeysMonotonic (\(C3 x y z) -> C3 (a+x) (b+y) (c+z)) ns

-- Unpacked 4-tuples ---------------------------------------------------

data C4 = C4 !Int !Int !Int !Int deriving (Eq, Ord)

toC4 :: (Int,Int) -> C4
toC4 (x,y) = C4 x y 0 0

neighborCount4 :: C4 -> Map C4 Int
neighborCount4 =
  let ns = Map.fromList [(C4 x y z w,1) | [x,y,z,w] <- tail (replicateM 4 [0,-1,1])]
  in \(C4 a b c d) -> Map.mapKeysMonotonic (\(C4 x y z w) -> C4 (a+x) (b+y) (c+z) (d+w)) ns
