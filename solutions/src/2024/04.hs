{-# Language ImportQualifiedPost, ParallelListComp #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/4>

>>> :{
:main + "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"
:}
18
9

-}
module Main (main) where

import Advent (getInputArray, arrIx)
import Advent.Coord (Coord(C), neighbors, origin)
import Data.Array.Unboxed (assocs)

-- | >>> :main
-- 2434
-- 1835
main :: IO ()
main =
 do input <- getInputArray 2024 4

    print (length [() | (k, 'X') <- assocs input
                      , d <- neighbors origin
                      , and [Just a == arrIx input b | a <- "MAS" | b <- iterate (d+) (d+k)]
                      ])

    print (length [() | (k, 'A') <- assocs input
                      , let cs = map (\x -> arrIx input (k+x)) corners
                      , target <- ["MMSS", "MSSM", "SSMM", "SMMS"]
                      , map Just target == cs])

-- | Diagonal corners of the origin in clockwise order.
corners :: [Coord]
corners = [C (-1) (-1), C (-1) 1, C 1 1, C 1 (-1)]
