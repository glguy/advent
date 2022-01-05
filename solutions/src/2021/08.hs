{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/8>

Figure out how the miswired segment display works.

-}
module Main (main) where

import Advent (countBy, format, fromDigits)
import Data.Bits (Bits(setBit))
import Data.Char (ord)
import Data.List (permutations, foldl')
import Data.Map (Map)
import Data.Map qualified as Map

-- | >>> :main
-- 355
-- 983030
main :: IO ()
main =
 do inp <- [format|2021 8 (%s&  %| %s& %n)*|]
    let outs = map solve inp
    print (countBy (`elem` [1,4,7,8]) (concat outs))
    print (sum (map (fromDigits 10) outs))

wires :: String
wires = ['a'..'g']

digits :: Map String Int
digits = Map.fromList (zip ["abcefg","cf","acdeg","acdfg","bcdf","abdfg","abdefg","acf","abcdefg","abcdfg"] [0..9])

-- | All the possible reassignments of wires
mappings :: [Map Int Int]
mappings =
  [ Map.mapKeys (toBitMask . map (assignment Map.!)) digits
    | wires' <- permutations wires
    , let assignment = Map.fromList (zip wires wires')
  ]

-- | Given a list of segment examples and outputs decode the outputs.
solve :: ([String], [String]) -> [Int]
solve (xs, ys) = head
  [ out
  | mapping <- mappings
  , let rewire x = Map.lookup (toBitMask x) mapping
  , Just out <- [traverse rewire xs *> traverse rewire ys]
  ]

-- | Convert the segment labels to a more efficient characteristic 'Int'
toBitMask :: String -> Int
toBitMask = foldl' (\acc x -> setBit acc (ord x - ord 'a')) 0
