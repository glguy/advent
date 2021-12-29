{-# Language ImportQualifiedPost, QuasiQuotes, BlockArguments #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/12>

Search around a cave visiting some caves more than others.

This solution makes the observation that we can optimize
away all the big caves. Big caves can never be connected
to other big caves or we'd have infinite cycles, and we
don't need to track anything about visiting a big cave.

-}
module Main (main) where

import Advent.Format (format)
import Advent.Memo (memo3)
import Advent.SmallSet qualified as SmallSet
import Control.Monad.Trans.State.Strict
import Data.Char (isUpper)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (mapAccumL)
import Data.Map (Map)
import Data.Map qualified as Map

-- | >>> :main
-- 3761
-- 99138
main :: IO ()
main =
 do inp <- compress . toAdj . label <$> [format|12 (%s-%s%n)*|]
    print (start inp False)
    print (start inp True)

-- | Compute directed edge map from a list of undirected edges.
toAdj :: [(Int, Int)] -> IntMap [Int]
toAdj inp = IntMap.fromListWith (++)
  [(x,[y]) | (a,b) <- inp, (x,y) <- [(a,b),(b,a)], y /= 0, x /= 1]

-- | Compute direct paths through a big cave to the next small cave.
compress :: IntMap [Int] -> IntMap (IntMap Int)
compress long = IntMap.filterWithKey (\k _ -> k >= 0) short
  where
    short = shorten <$> long
    shorten xs = IntMap.unionsWith (+)
      [if x >= 0 then IntMap.singleton x 1 else short IntMap.! x | x <- xs]

-- | Search the cave exploration given the directed edges and a
-- flag if we're allowed to visit a small cave an extra time.
start :: IntMap (IntMap Int) -> Bool -> Int
start paths = go 0 SmallSet.empty
  where
    go = memo3 \here seen extra ->
      let
        f next
          | next == 1 = 1
          | not (SmallSet.member next seen) = go next (SmallSet.insert next seen) extra
          | extra     = go next seen False
          | otherwise = 0
      in sum [v * f k | (k,v) <- IntMap.toList (paths IntMap.! here)]

-- | Map all the cave names to integers. Use negative integers for big caves.
-- @start@ always gets assigned @0@ and @end@ gets @1@
label :: [(String, String)] -> [(Int,Int)]
label xs =
  evalState
    (traverse (both (state . label1)) xs)
    (Map.fromList [("start",0), ("end",1)])

-- | Assigns a unique integer label for each cave name. Big cave names
-- are assigned negative integers.
label1 :: String -> Map String Int -> (Int, Map String Int)
label1 x m =
  case Map.lookup x m of
    Just i  -> (i, m)
    Nothing -> (i, Map.insert x i m)
      where i = if isUpper (head x) then -Map.size m else Map.size m

-- | Traverse over the first and second components of a pair.
both :: Applicative f => (a -> f b) -> (a,a) -> f (b,b)
both f (x,y) = (,) <$> f x <*> f y
