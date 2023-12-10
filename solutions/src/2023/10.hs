{-# Language LambdaCase, ImportQualifiedPost, TransformListComp #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/10>

>>> :{
:main +
".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
"
:}
70
8

>>> :{
:main +
"FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
"
:}
80
10

-}
module Main (main) where

import Advent (getInputMap)
import Advent.Coord (cardinal, invert, invert', south, north, west, turnRight, Coord)
import Advent.Search (dfsN, dfsOn)
import Data.List (nub, (\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Parse the input and print out answers to both parts.
--
-- >>> :main
-- 6907
-- 541
main :: IO ()
main =
 do input <- getInputMap 2023 10
    let (start, dir0) = pickStart input
    let route = [(here,dir) | (dir,here) <- dfsOn snd (step input) (dir0, dir0+start)]

    let pipe = Set.fromList (map fst route)
    let containable = Map.keysSet input `Set.difference` pipe
    let candidates = Set.fromList (concatMap (rightof input) route) `Set.difference` pipe
    let contained = dfsN (openNeighbors containable) (Set.toList candidates)

    print (length route `quot` 2)
    print (length contained)

pickStart :: Map Coord Char -> (Coord, Coord)
pickStart input = head $
  [ (k, dir)
  | (k, 'S') <- Map.assocs input
  , (dir, ok) <- [(south, "L|J"), (north, "F|7"), (west,"7-J")]
  , let next = Map.findWithDefault '.' (k+dir) input
  , next `elem` ok
  ]

openNeighbors :: Set Coord -> Coord -> [Coord]
openNeighbors input x = [y | y <- cardinal x, Set.member y input]

step :: Map Coord Char -> (Coord, Coord) -> [(Coord, Coord)]
step inp (dir, here) =
  [(dir', here + dir') | let dir' = pipeEffect (inp Map.! here) dir]

rightof :: Map Coord Char -> (Coord, Coord) -> [Coord]
rightof input (here, dir) =
  nub [turnRight d + here | d <- [dir, pipeEffect (input Map.! here) dir]]

pipeEffect :: Char -> Coord -> Coord
pipeEffect = \case
  'S' -> id
  '-' -> id
  '|' -> id
  '7' -> invert
  'L' -> invert
  'J' -> invert'
  'F' -> invert'
  _   -> error "bad pipe character"
