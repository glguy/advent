{-# Language QuasiQuotes, ImportQualifiedPost #-}
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

import Advent
import Advent.Coord (Coord(C), neighbors, turnLeft, origin)
import Data.Map qualified as Map

-- | >>> :main
-- 2434
-- 1835
main :: IO ()
main =
 do input <- getInputMap 2024 4
    let index = traverse (`Map.lookup` input)

    print (length [() | k <- Map.keys input
                      , d <- neighbors origin
                      , let xs = take 4 (iterate (d+) k)
                      , Just "XMAS" == index xs])

    print (length [() | k <- Map.keys input
                      , ds <- take 4 (iterate (map turnLeft) xshape)
                      , let xs = map (k+) ds
                      , Just "AMMSS" == index xs])

xshape :: [Coord]
xshape = origin : (C <$> [-1, 1] <*> [-1, 1])
