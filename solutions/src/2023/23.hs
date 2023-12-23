{-# Language BangPatterns, ImportQualifiedPost #-}
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
    let input1 = buildPaths input part1
    let input2 = buildPaths input part2
    print (maximum (enum ymax (C 0 1) input1 0))
    print (maximum (enum ymax (C 0 1) input2 0))

-- | Generate all the possible distances from the start to the end.
enum :: Int -> Coord -> Map Coord [(Coord, Int)] -> Int -> [Int]
enum !ymax !here edges !dist
  | coordRow here == ymax = [dist]
  | otherwise =
   do let edges' = Map.delete here edges
      (next, cost) <- Map.findWithDefault [] here edges
      enum ymax next edges' (dist + cost)

buildPaths ::
  UArray Coord Char ->
  (Char -> Coord -> Bool) ->
  Map Coord [(Coord, Int)]
buildPaths input isOpen = go Map.empty (C 0 1)
  where
    (_, C ymax _) = bounds input

    go acc x
      | Map.member x acc = acc
      | otherwise = foldl go (Map.insert x reachable acc) (map fst reachable)
      where
        reachable =
         do c <- adj input isOpen x
            walk c x 1

    walk here there dist =
      case delete there (adj input isOpen here) of
        [next] | coordRow next /= ymax -> walk next here (dist+1)
        _                              -> [(here, dist)]

adj :: UArray Coord Char -> (Char -> Coord -> Bool) -> Coord -> [Coord]
adj input isOpen here =
  [ next
  | next <- cardinal here
  , cell <- arrIx input next
  , isOpen cell (next - here)
  ]

part1 :: Char -> Coord -> Bool
part1 c dir =
  case c of
    '#' -> False
    '>' -> dir == east
    'v' -> dir == south
    '^' -> dir == north
    '<' -> dir == west
    _   -> True

part2 :: Char -> Coord -> Bool
part2 c _ =
  case c of
    '#' -> False
    _   -> True
