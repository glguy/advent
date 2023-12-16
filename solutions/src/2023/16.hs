{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/16>

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

import Advent (getInputArray, arrIx, ordNub)
import Advent.Coord (east, invert, invert', north, origin, south, west, Coord(C))
import Advent.Search (dfs)
import Data.Array.Unboxed (inRange, bounds, UArray )

-- | Parse the input grid and print answers to both parts.
--
-- >>> :main
-- 7979
-- 8437
main :: IO ()
main =
 do input <- getInputArray 2023 16
    print (solve input (origin, east))
    print (maximum (map (solve input) (edges (bounds input))))

-- | Count the number of energized tiles given an input beam.
solve :: UArray Coord Char -> (Coord, Coord) -> Int
solve input = length . ordNub . map fst . dfs (step input)

-- | Find all the incoming light possibilities for part 2
edges :: (Coord, Coord) -> [(Coord, Coord)]
edges (C y1 x1, C y2 x2) =
  [(C y1 x, south) | x <- [x1..x2]] ++
  [(C y2 x, north) | x <- [x1..x2]] ++
  [(C y x1, east ) | y <- [y1..y2]] ++
  [(C y x2, west ) | y <- [y1..y2]]

-- | Advance a light beam once cell forward and track its
-- resulting outgoing beams.
step :: UArray Coord Char -> (Coord, Coord) -> [(Coord, Coord)]
step input (here, dir) =
  [ (here', dir')
  | dir' <-
    case arrIx input here of
      Just '\\'                           -> [invert dir]
      Just '/'                            -> [invert' dir]
      Just '.'                            -> [dir]
      Just '|' | dir `elem` [north,south] -> [dir]
               | otherwise                -> [north,south]
      Just '-' | dir `elem` [east,west]   -> [dir]
               | otherwise                -> [east, west]
      _                                   -> []
  , let here' = here + dir'
  , inRange (bounds input) here'
  ]
