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
import Data.Set (Set)

-- | >>> :main
-- 303
-- 1045
main :: IO ()
main =
 do input <- getInputArray 2024 8
    let antennaGroups = Map.elems (Map.fromListWith (++) [(v, [k]) | (k, v) <- assocs input, v /= '.'])
        inInput = inRange (bounds input)
    print (length (antinodes antennaGroups (nodeNeighbors inInput)))
    print (length (antinodes antennaGroups (nodeLine inInput)))

-- | Generate all the unique antinodes given the groups of antennas and a way to
-- generate nodes of a pairs of antennas.
antinodes ::
    [[Coord]]                   {- ^ groups of antennas                    -} ->
    (Coord -> Coord -> [Coord]) {- ^ function to generate nodes for a pair -} ->
    Set Coord                   {- ^ set of unique nodes generated         -}
antinodes antennaGroups generate =
  Set.fromList
    [ node
    | antennaGroup <- antennaGroups
    , x:ys <- tails antennaGroup
    , y    <- ys
    , node <- generate x y
    ]

-- | Neighbors from part one where antinodes are on either side of a
-- pair of nodes.
nodeNeighbors :: (Coord -> Bool) -> Coord -> Coord -> [Coord]
nodeNeighbors p x y = filter p [2 * y - x, 2 * x - y]

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
