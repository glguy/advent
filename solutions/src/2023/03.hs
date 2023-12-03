{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/3>

>>> :{
:main +
"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
:}
4361
467835

-}
module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Char (isDigit)

import Advent (getInputMap, ordNub)
import Advent.Coord (Coord, left, neighbors, right)

-- | Parse the input schematic and print answers to both parts.
--
-- >>> :main
-- 527144
-- 81463996
main :: IO ()
main =
 do input <- getInputMap 2023 3
    let
      lkp i = Map.findWithDefault '.' i input

      -- Map of each part in the schematic to the list of adjacent part numbers
      partNumbers :: Map Coord [Int]
      partNumbers = Map.fromListWith (++)
          [ (part, [read (map lkp cs)])
          | (c,n) <- Map.assocs input
          , isDigit n, not (isDigit (lkp (left c))) -- left-boundary of number
          , let cs = takeWhile (isDigit . lkp) (iterate right c)
          , part <- ordNub (concatMap neighbors cs)
          , isPart (lkp part)
          ]

    print (sum (fmap sum partNumbers))
    print (sum [a * b | (c, [a,b]) <- Map.assocs partNumbers, '*' == lkp c])

-- | Things that aren't digits or periods.
isPart :: Char -> Bool
isPart x = not (isDigit x) && x /= '.'
