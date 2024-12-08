{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/8>

>>> :{
:main + "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"
:}
14
34

-}
module Main (main) where

import Advent (getInputArray)
import Advent.Coord (Coord(C))
import Data.Array.Unboxed (assocs, bounds, inRange)
import Data.List (tails)
import Data.Map qualified as Map
import Data.Set qualified as Set

-- | >>> :main
-- 303
-- 1045
main :: IO ()
main =
 do input <- getInputArray 2024 8
    let antennaGroups = Map.elems (Map.fromListWith (++) [(v, [k]) | (k, v) <- assocs input, v /= '.'])
    print (length (Set.fromList
      [ node
      | antennaGroup <- antennaGroups
      , x:ys <- tails antennaGroup
      , y    <- ys
      , node <- [2 * y - x, 2 * x - y]
      , inRange (bounds input) node
      ]))
    print (length (Set.fromList
      [ node
      | antennaGroup <- antennaGroups
      , x:ys <- tails antennaGroup
      , y    <- ys
      , node <- nodeLine (inRange (bounds input)) x y
      ]))

-- | Generate all the points on the line defined by two coordinates
-- that fit inside a bounding box predicate.
nodeLine :: (Coord -> Bool) -> Coord -> Coord -> [Coord]
nodeLine p a b =
    takeWhile p (iterate (step +) a) ++
    takeWhile p (iterate (subtract step) (a - step))
  where
    C dy dx                  = a - b
    com | dx == 0 || dy == 0 = 1
        | otherwise          = gcd dy dx
    step                     = C (dy `quot` com) (dx `quot` com)
