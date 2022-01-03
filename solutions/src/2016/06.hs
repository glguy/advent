{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/6>

-}
module Main where

import           Advent
import           Data.List
import qualified Data.Map as Map
import           Data.Ord

main :: IO ()
main =
  do input <- getInputLines 6
     putStrLn (decode id   input)
     putStrLn (decode Down input)

decode :: Ord a => (Int -> a) -> [String] -> String
decode f xs = mostCommon f <$> transpose xs

mostCommon :: (Ord a, Ord b) => (Int -> b) -> [a] -> a
mostCommon f = fst . maximumBy (comparing (f . snd)) . tally

tally :: Ord a => [a] -> [(a,Int)]
tally xs = Map.toList (Map.fromListWith (+) [(x,1) | x <- xs])
