{-# Language LambdaCase, ImportQualifiedPost, BlockArguments #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/10>

This solution finds the contained area using
<https://en.wikipedia.org/wiki/Shoelace_formula>.

>>> :{
:main +
".....
.S-7.
.|.|.
.L-J.
.....
"
:}
4
1

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
import Advent.Coord (invert, invert', south, north, west, Coord(C))
import Advent.Search (dfsOn)
import Data.Map (Map)
import Data.Map qualified as Map

-- | Parse the input and print out answers to both parts.
--
-- >>> :main
-- 6907
-- 541
main :: IO ()
main =
 do input <- getInputMap 2023 10
    let (start, dir0) = pickStart input
        route = getLoop (map fst (iterate (step input) (start, dir0)))
        perimeter = length route
    print (perimeter `quot` 2)
    print (abs (polyareaRect route) - perimeter `quot` 2 + 1)

pickStart :: Map Coord Char -> (Coord, Coord)
pickStart input = head $
  [ (k, dir)
  | (k, 'S') <- Map.assocs input
  , (dir, ok) <- [(south, "L|J"), (north, "F|7"), (west,"7-J")]
  , let next = Map.findWithDefault '.' (k + dir) input
  , next `elem` ok
  ]

getLoop :: Eq a => [a] -> [a]
getLoop (x:xs) = x : takeWhile (x /=) xs

step :: Map Coord Char -> (Coord, Coord) -> (Coord, Coord)
step inp (here, dir) =
  let here' = here + dir in
  (here', pipeEffect (inp Map.! here') dir)

pipeEffect :: Char -> Coord -> Coord
pipeEffect = \case
  '-' -> id; '|' -> id
  '7' -> invert ; 'L' -> invert
  'J' -> invert'; 'F' -> invert'
  c   -> error ("bad pipe character: " ++ show c)

-- | Area of a polygon using Shoelace formula.
polyareaRect :: [Coord] -> Int
polyareaRect xs = f 0 (xs ++ take 1 xs)
  where
    f acc (C y1 x1 : cs@(C y2 x2 : _)) = f (acc + x1 * y2 - x2 * y1) cs
    f acc _ = acc `quot` 2
