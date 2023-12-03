{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/3>

-}
module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Char (isDigit)
import Data.List (find)

import Advent (getInputMap)
import Advent.Coord (Coord, left, neighbors, right)

-- |
--
-- >>> :main
-- 527144
-- 81463996
main :: IO ()
main =
 do input <- getInputMap 2023 3
    let 
      lkp i = Map.findWithDefault '.' i input

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

isSymbol :: Char -> Bool
isSymbol x = not (isDigit x) && x /= '.'
