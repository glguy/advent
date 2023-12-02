{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/2>

This problem has us observe multiple rounds of a game where marbles
are drawn from a bag. We have to compute the fewest number of each
color marble that are possible to support each game trace.

>>> :{
:main + "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"
:}
8
2286

-}
module Main where

import Data.Map (Map)
import Data.Map qualified as Map

import Advent (format)

-- | Parse the input and print the answers to both parts.
--
-- >>> :main
-- 2169
-- 60948
main :: IO ()
main =
 do input <- [format|2023 2 (Game %d: (%d %s)&(, )&(; )%n)*|]
    let summaries = [(i, summarizeGame rounds) | (i, rounds) <- input]
    print (sum [i | (i, summary) <- summaries, all part1 (Map.assocs summary)])
    print (sum [product m | (_, m) <- summaries])

-- | Find the minimum marbles needed to play a whole game
summarizeGame :: [[(Int, String)]] -> Map String Int
summarizeGame = Map.unionsWith max . map summarizeRound

-- | Find the minimum marbles needed to play round in a game
summarizeRound :: [(Int, String)] -> Map String Int
summarizeRound r = Map.fromListWith (+) [(color, count) | (count, color) <- r]

-- | Check if the number of marbles of a certain color is below the threshold for part 1
part1 :: (String, Int) -> Bool
part1 ("red"  , v) = v <= 12
part1 ("green", v) = v <= 13
part1 ("blue" , v) = v <= 14
part1 _            = False
