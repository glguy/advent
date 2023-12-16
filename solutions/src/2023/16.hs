{-# Language QuasiQuotes, ImportQualifiedPost, BangPatterns, LambdaCase #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/16>

This is a pretty straight forward breadth-first traversal of the 
state space. I represent nodes of the "graph" being searched
as pairs of a location and a direction vector. At each step
the location is used to look up the tile and the direction
vector is used to compute reflections and splits.

Optimizations:

- Parallelize the search in part 2
- Only track seen states for beam splitters
- Count visited locations with an array instead of a Set
- Pack photon states into an Int to make seen set lookups faster

>>> :{
:main +
".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....
"
:}
46
51

-}
module Main (main) where

import Advent (getInputArray, arrIx, countBy)
import Advent.Coord (east, invert, invert', north, origin, south, west, coordCol, coordRow, Coord(C))
import Control.Parallel.Strategies (parMap, rpar)
import Data.Array.Unboxed (inRange, bounds, UArray, (!), accumArray, elems)
import Data.IntSet qualified as IntSet

type Photon = (Coord, Coord) -- location, velocity

-- | Parse the input grid and print answers to both parts.
--
-- >>> :main
-- 7979
-- 8437
main :: IO ()
main =
 do input <- getInputArray 2023 16
    print (solve input (origin, east))
    print (maximum (parMap rpar (solve input) (edges (bounds input))))

-- | Count the number of energized tiles given an input beam.
solve :: UArray Coord Char -> Photon -> Int
solve input = coverage (bounds input) . dfsOn isSplitter photonRep (step input)
  where
    -- branching only happens at splitters, so only bother avoiding
    -- duplication of work when leaving them
    isSplitter (here, _) = input!here `elem` "-|"

-- | Use a more compact representative of the state space to speed
-- up the visited test. This saves about a 3rd of the runtime as without.
photonRep :: Photon -> Int
photonRep (C y x, C dy dx) = y * 4096 + x * 16 + (dy+1) * 2 + (dx+1)

-- | Find all the incoming light possibilities for part 2
edges :: (Coord, Coord) {- ^ bounds -} -> [Photon]
edges (C y1 x1, C y2 x2) =
  [(C y1 x, south) | x <- [x1..x2]] ++
  [(C y2 x, north) | x <- [x1..x2]] ++
  [(C y x1, east ) | y <- [y1..y2]] ++
  [(C y x2, west ) | y <- [y1..y2]]

-- | Advance a light beam once cell forward and track its
-- resulting outgoing beams.
step :: UArray Coord Char -> Photon -> [Photon]
step input (here, dir) =
  [ (here', dir')
  | dir' <-
    case arrIx input here of
      Nothing                      -> []
      Just '\\'                    -> [invert dir]
      Just '/'                     -> [invert' dir]
      Just '|' | coordRow dir == 0 -> [north, south]
      Just '-' | coordCol dir == 0 -> [east, west]
      _                            -> [dir]
  , let here' = here + dir'
  , inRange (bounds input) here'
  ]

-- This is a copy of Advent.Search.dfsOn but augmented with
-- a predicate identifying the states that cause branching
-- so we don't bother deduplicating states that don't matter.
dfsOn :: (a -> Bool) -> (a -> Int) -> (a -> [a]) -> a -> [a]
dfsOn p rep next start = loop IntSet.empty [start]
  where
    loop !seen = \case
      [] -> []
      x : q
        | IntSet.member r seen ->     loop seen  q
        | otherwise            -> x : loop seen' q'
        where
          r     = rep x
          seen' = if p x then IntSet.insert r seen else seen
          q'    = next x ++ q

-- A more efficient way to count the number of unique coordinates
-- in the energized laser path.
coverage :: (Coord, Coord) {- ^ bounds -} -> [Photon] -> Int
coverage bnds path = countBy id (elems a)
  where
    a :: UArray Coord Bool
    a = accumArray (\_ _ -> True) False bnds [(p, ()) | (p, _v) <- path]