{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/7>

>>> :{
:main +
".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"
:}
21
40

-}
module Main (main) where

import Advent ( getInputArray, arrIx )
import Advent.Coord ( above, left, right, Coord(..) )
import Data.Array.Unboxed (range, (!), elems, indices, listArray, bounds, UArray, Array)

-- | >>> :main
-- 1541
-- 80158285728929
main :: IO ()
main =
 do input <- getInputArray 2025 7
    let beam = simulateBeam input
    
    print (length [() | ('^', n) <- elems input `zip` elems beam, n > 0])
    
    let (C _ loc, C hir hic) = bounds input
    print (sum [beam ! i | i <- range (C hir loc, C hir hic) ])

simulateBeam :: UArray Coord Char -> Array Coord Int
simulateBeam input = counts
  where
    check i xs = if arrIx input i `elem` map Just xs then counts ! i else 0
    counts = listArray (bounds input)
      [ if 'S' == input ! i then 1 else u + l + r
      | i <- indices input
      , let u = check (above i) "S."
            l = check (above (left i)) "^"
            r = check (above (right i)) "^"
      ]
