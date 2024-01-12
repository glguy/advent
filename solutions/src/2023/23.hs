{-# Language BangPatterns, LambdaCase, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/23>

A brute-forced approach. First extract the graph of
intersections and then just enumerate all the possible
paths between them that reach the exit.

>>> :{
:main +
"#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
"
:}
94
154

-}
module Main (main) where

import Advent (getInputArray, arrIx)
import Advent.Coord (cardinal, coordRow, east, north, south, west, Coord(C))
import Data.Array.Unboxed (bounds, UArray)
import Data.List (delete)
import Data.Map (Map)
import Data.Map qualified as Map

main :: IO ()
main =
 do input <- getInputArray 2023 23
    let (_, C ymax _) = bounds input
    let solve = maximum . enum ymax (C 0 1) 0 . buildPaths input

    print (solve part1)
    print (solve part2)

-- | Generate all the possible distances from the start to the end.
enum :: Int -> Coord -> Int -> Map Coord [(Coord, Int)] -> [Int]
enum !ymax !here !dist edges
  | coordRow here == ymax = [dist]
  | otherwise =
   do let edges' = Map.delete here edges
      (next, cost) <- Map.findWithDefault [] here edges
      enum ymax next (dist + cost) edges'

-- | Build a map of locations and distances reachable from each key.
buildPaths ::
  UArray Coord Char       {- ^ input grid        -} ->
  (Coord -> Char -> Bool) {- ^ adjacency rule    -} ->
  Map Coord [(Coord, Int)]
buildPaths input isOpen = go Map.empty (C 0 1)
  where
    (_, C ymax _) = bounds input

    go acc c
      | Map.member c acc = acc -- already computed, skip
      | otherwise = foldl go (Map.insert c reachable acc) (map fst reachable)
      where
        reachable = map (walk 1 c) (adj input isOpen c)

    -- find the next intersection in this direction and track the distance to it
    walk dist prev cur
      | coordRow cur /= ymax                         -- not the terminal location
      , [next] <- delete prev (adj input isOpen cur) -- only one next location
      = walk (dist + 1) cur next                     -- keep walking

      | otherwise = (cur, dist)                      -- record interesting location

-- | Return all the coordinates that are adjacent to this one.
adj :: UArray Coord Char -> (Coord -> Char -> Bool) -> Coord -> [Coord]
adj input isOpen cur =
  [ next
  | next <- cardinal cur
  , char <- arrIx input next
  , isOpen (next - cur) char
  ]

-- | Adjacency rule that respects slope characters.
part1 :: Coord -> Char -> Bool
part1 dir = \case
  '#' -> False
  '>' -> dir == east
  'v' -> dir == south
  '^' -> dir == north
  '<' -> dir == west
  _   -> True

-- | Adjacency rule that ignores slope characters.
part2 :: Coord -> Char -> Bool
part2 _ = ('#' /=)
