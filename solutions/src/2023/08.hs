{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/8>

-}
module Main where

import Advent
import Data.Map (Map)
import Data.Map qualified as Map

data D = DL | DR

stageTH

-- |
--
-- >>> :main
-- 20777
-- 13289612809129
main :: IO ()
main =
 do (steps, nodes) <- [format|2023 8 @D*%n%n(%s = %(%s, %s%)%n)*|]
    let steps' = cycle steps
    let nodes' = Map.fromList [(k,(a,b)) | (k,a,b) <- nodes]
    print (pathLength part1 nodes' steps' "AAA")
    print (foldl1 lcm [ pathLength part2 nodes' steps' start
                      | start <- Map.keys nodes', last start == 'A'])

part1, part2 :: String -> Bool
part1 x = "ZZZ" == x
part2 x = 'Z' == last x

pathLength :: Ord a => (a -> Bool) -> Map a (a, a) -> [D] -> a -> Int
pathLength p nodes = go 0
  where
    go n (dir : dirs) here
      | p here = n
      | otherwise =
      case (dir, nodes Map.! here) of
        (DL, (l, _)) -> go (n + 1) dirs l
        (DR, (_, r)) -> go (n + 1) dirs r
