{-# Language ImportQualifiedPost, TransformListComp #-}
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
module Main (main) where

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
    let numbers = extractNumbers input
    print (sum [partNo | (partNo, _:_) <- numbers])
    print (sum [a * b | [a, b] <- gearNumbers numbers])

-- | Extract the numbers from the diagram and the parts adjacent to them.
extractNumbers :: Map Coord Char -> [(Int, [(Coord, Char)])]
extractNumbers input =
  [ (read digits, partsNear cs)
  | (c, digit) <- Map.assocs input
  , isDigit digit, not (isDigit (lkp (left c))) -- left-boundary of number
  , let (cs, digits) = unzip (numbersAfter c)
  ]
  where
    lkp i = Map.findWithDefault '.' i input
    numbersAfter start =
      [ (c, digit)
      | c <- iterate right start
      , let digit = lkp c
      , then takeWhile by isDigit digit
      ]
    partsNear cs =
      [ (c, sym)
      | c <- ordNub (concatMap neighbors cs)
      , let sym = lkp c
      , isPart sym
      ]

-- | Make lists of the numbers next to each gear in the schematic
gearNumbers :: [(Int, [(Coord, Char)])] -> [[Int]]
gearNumbers numbers =
  Map.elems (Map.fromListWith (++)
    [(part, [partNo]) | (partNo, parts) <- numbers, (part, '*') <- parts])

-- | Things that aren't digits or periods.
isPart :: Char -> Bool
isPart x = not (isDigit x) && x /= '.'
