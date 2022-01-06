{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/13>

-}
module Main where

import Advent (format)
import Advent.Coord (Coord(..), cardinal)
import Advent.Search (bfsOn)
import Data.Bits (Bits(popCount))

data Entry = Entry { entrySteps :: !Int, entryCoord :: Coord }
  deriving (Eq, Show)

-- | >>> :main
-- 92
-- 124
main :: IO ()
main =
 do input <- [format|2016 13 %u%n|]
    let entries = bfsOn entryCoord (nextEntries input) initialEntry
    print $ head [steps | Entry steps (C 39 31) <- entries]
    print $ length $ takeWhile (<= 50) $ map entrySteps entries

initialEntry :: Entry
initialEntry = Entry 0 (C 1 1)

isValidCoord :: Int -> Coord -> Bool
isValidCoord input (C y x) =
  x >= 0 && y >= 0 &&
  even (popCount (x*x + 3*x + 2*x*y + y + y*y + input))

nextEntries :: Int -> Entry -> [Entry]
nextEntries input (Entry steps coord) =
  [Entry (steps+1) c | c <- cardinal coord, isValidCoord input c]
