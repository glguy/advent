{-# Language BlockArguments, ImportQualifiedPost, MonadComprehensions #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/12>

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
import Advent.Coord (Coord, cardinal, above, right, below, left)
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
    let rs = regions input
    print (sum (map (\x -> perimeter x * length x) rs))
    print (sum (map (\x -> walls     x * length x) rs))

-- | Return a list of regions in the input map.
regions :: Map Coord Char -> [Set Coord]
regions = unfoldr \input ->
  [ (region, Map.withoutKeys input region)
  | (start, label) <- Map.lookupMin input
  , let region = fill step start
        step i = [j | j <- cardinal i, Map.lookup j input == Just label]
  ]

-- | Find the perimeter length of a region.
perimeter :: Set Coord -> Int
perimeter xs = length [() | x <- Set.toList xs, y <- cardinal x, y `Set.notMember` xs]

-- | Compute the number of walls needed to surround a region by looking for
-- the corners of the region.
walls :: Set Coord -> Int
walls xs
  = countBy (corner left  above) xs + countBy (corner above right) xs
  + countBy (corner below left ) xs + countBy (corner right below) xs
  where
    corner dir1 dir2 x = open dir1 && (open dir2 || not (open (dir1 . dir2)))
      where open dir = dir x `Set.notMember` xs
