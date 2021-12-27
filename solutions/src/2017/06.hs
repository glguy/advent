{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main where

import Advent
import Data.Map qualified as Map
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V

main :: IO ()
main =
  do input <- [format|6 %u&%t%n|]
     print (solve (V.fromList input))

-- | Compute both parts of Day 6
--
-- >>> solve (V.fromList [0,2,7,0])
-- (5,4)
solve :: Vector Int -> (Int,Int)
solve = findLoop . iterate step

-- | Computes the steps until a state repeats and also the length of the loop
--
-- >>> findLoop [1,2,3,4,5,6,7,5]
-- (7,3)
-- >>> findLoop [1,1]
-- (1,1)
-- >>> findLoop [0,1,1]
-- (2,1)
findLoop :: Ord a => [a] -> (Int,Int)
findLoop = go Map.empty
  where
    go seen (x:xs) =
      let n = Map.size seen in
      case Map.lookup x seen of
        Nothing -> go (Map.insert x n seen) xs
        Just i  -> (n, n-i)

-- | Given a vector representing the memory banks compute the new memory bank
-- layout.
--
-- >>> step (V.fromList [0,2,7,0])
-- [2,4,1,2]
-- >>> step (V.fromList [2,4,1,2])
-- [3,1,2,3]
-- >>> step (V.fromList [3,1,2,3])
-- [0,2,3,4]
step :: Vector Int -> Vector Int
step xs = V.accum (+) xs ((i, -mx) : [ (j`rem`n, 1) | j <- [i+1 .. i+mx]])
  where
    mx     = V.maximum xs
    Just i = V.elemIndex mx xs
    n      = V.length xs
