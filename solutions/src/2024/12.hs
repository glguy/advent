{-# Language BlockArguments, ImportQualifiedPost, MonadComprehensions #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/12>

This solution represents edges of regions as the pair of a square
location and a unit vector that points in the direction of one
of the sides of that square.

>>> :{
:main + "AAAA
BBCD
BBCC
EEEC
"
:}
140
80

>>> :{
:main + "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
"
:}
772
436

>>> :{
:main + "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
"
:}
1930
1206

-}
module Main (main) where

import Advent (getInputMap, countBy)
import Advent.Coord (Coord, cardinal, turnLeft)
import Advent.Search (fill)
import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 1546338
-- 978590
main :: IO ()
main =
 do input <- getInputMap 2024 12
    let rs = [(length r, edges r) | r <- regions input]
    print (sum [area * length es               | (area, es) <- rs])
    print (sum [area * countBy (isStart es) es | (area, es) <- rs])

-- | Return a list of regions in the input map.
regions :: Map Coord Char -> [Set Coord]
regions = unfoldr \input ->
  [ (region, Map.withoutKeys input region)
  | (start, label) <- Map.lookupMin input
  , let region = fill step start
        step i = [j | j <- cardinal i, Map.lookup j input == Just label]
  ]

-- | Find the perimeter edges of a region.
edges :: Set Coord -> Set (Coord, Coord)
edges xs = Set.fromList
  [(x, y - x) | x <- Set.toList xs, y <- cardinal x, Set.notMember y xs]

-- | Predicate for edges that are the "beginning" of a edge
-- and thus represent their whole "side".
isStart :: Set (Coord, Coord) -> (Coord, Coord) -> Bool
isStart xs (p, d) = Set.notMember (p + turnLeft d, d) xs
