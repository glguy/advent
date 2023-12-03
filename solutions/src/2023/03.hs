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
import Data.List (find)

import Advent (getInputMap)
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
      partMap :: Map Coord [Int]
      partMap = Map.fromListWith (++)
          [ (part, [read (map lkp cs)])
          | (c,n) <- Map.assocs input
          , isDigit n
          , not (isDigit (lkp (left c)))
          , let cs = takeWhile (isDigit . lkp) (iterate right c)
          , Just part <- [find (isSymbol . lkp) (concatMap neighbors cs)]
          ]

    print (sum (fmap sum partMap))
    print (sum [a * b | (c, [a,b]) <- Map.assocs partMap, '*' == lkp c])

-- | Things that aren't digits or periods.
isSymbol :: Char -> Bool
isSymbol x = not (isDigit x) && x /= '.'
