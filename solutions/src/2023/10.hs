
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
"-L|F7
7S-7|
L|7||
-L-J|
L|-JF
"
:}
4
1

>>> :{
:main +
"7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
"
:}
8
1

>>> :{
:main +
"...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
"
:}
23
4

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

import Advent (getInputArray, arrIx)
import Advent.Coord (invert, invert', south, north, west, manhattan, Coord(C))
import Data.Array.Unboxed (UArray, (!), assocs)
import Data.List (tails)

-- | Parse the input and print out answers to both parts.
--
-- >>> :main
-- 6907
-- 541
main :: IO ()
main =
 do input <- getInputArray 2023 10
    let route = uncurry (follow input) (pickStart input)
        perimeter = sum (zipWith manhattan route (tail route))
    print (perimeter `quot` 2)
    print (abs (polyareaRect route) - perimeter `quot` 2 + 1)

-- | Determine the location of the start cell and a valid direction
-- leaving it.
pickStart :: UArray Coord Char -> (Coord, Coord)
pickStart input = head
  [ (c, dir)
  | (c, 'S')  <- assocs input
  , (dir, ok) <- [(south, "L|J"), (north, "F|7"), (west,"7-J")]
  , next      <- arrIx input (c + dir)
  , next `elem` ok
  ]

-- | Trace out the closed loop from start to start
follow :: UArray Coord Char -> Coord -> Coord -> [Coord]
follow input start dir0 = start : go dir0 start 
  where
    go dir prev =
      let here = dir + prev in
      case input ! here of
        'S' -> [here]
        '7' -> here : go (invert  dir) here
        'L' -> here : go (invert  dir) here
        'J' -> here : go (invert' dir) here
        'F' -> here : go (invert' dir) here
        _   ->        go dir           here

-- | Area of a polygon using Shoelace formula on a closed loop
-- in a clockwise direction.
polyareaRect :: [Coord] -> Int
polyareaRect xs = sum [x1 * y2 - x2 * y1 | C y1 x1 : C y2 x2 : _ <- tails xs] `quot` 2
