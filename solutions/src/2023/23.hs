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

-}
module Main (main) where

import Advent (getInputArray, arrIx)
import Advent.Coord (cardinal, coordRow, east, north, south, west, Coord(C))
import Data.Array.Unboxed (bounds, UArray)
import Data.Map (Map)
import Data.Map qualified as Map

main :: IO ()
main =
 do input <- getInputArray 2023 23
    let (_, C ymax _) = bounds input
    let input1 = buildPaths input part1
    let input2 = buildPaths input part2
    print (enum ymax (C 0 1) input1 0)
    print (enum ymax (C 0 1) input2 0)

enum :: Int -> Coord -> Map Coord [(Coord, Int)] -> Int -> Int
enum !ymax !here edges !dist
  | coordRow here == ymax = dist
  | Just nexts <- Map.lookup here edges
  = maximum [ enum ymax next edges' (dist + cost)
            | let edges' = Map.delete here edges
            , (next, cost) <- nexts
            ]
  | otherwise = 0

buildPaths ::
  UArray Coord Char ->
  (Char -> Coord -> Bool) ->
  Map Coord [(Coord, Int)]
buildPaths input isOpen = go [C 0 1] Map.empty
  where
    (_, C ymax _) = bounds input

    adj here =
      [ next
      | next <- cardinal here
      , cell <- arrIx input next
      , isOpen cell (next - here)
      ]

    go [] acc = acc
    go (x:xs) acc
      | Map.member x acc = go xs acc
      | otherwise = go (map fst nodes ++ xs) (Map.insert x nodes acc)
      where
        nodes =
         do c <- adj x
            walk c x 1

    walk here there dist =
      case filter (there /=) (adj here) of
        [next] | coordRow next /= ymax -> walk next here (dist+1)
        _ -> [(here, dist)]

part1 :: Char -> Coord -> Bool
part1 c dir =
  case c of
    '.' -> True
    '>' -> dir == east
    'v' -> dir == south
    '^' -> dir == north
    '<' -> dir == west
    _   -> False

part2 :: Char -> Coord -> Bool
part2 c _ =
  case c of
    '.' -> True
    '>' -> True
    'v' -> True
    '^' -> True
    '<' -> True
    _   -> False
