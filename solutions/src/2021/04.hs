{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/4>

Today we played Bingo and picked the first and last winning cards

-}
module Main (main) where

import Advent (format)
import Data.List (partition, transpose)

type Board = [[Int]]

-- | >>> :main
-- 49686
-- 26878
main :: IO ()
main =
 do (calls, boards) <- [format|2021 4 %u&,%n(%n(( *%u)+%n)+)*|]
    let outcomes = play calls boards
    print (head outcomes)
    print (last outcomes)

-- | Given the called numbers and initial boards return a list of
-- winning scores in order of winning.
play :: [Int] -> [Board] -> [Int]
play [] _ = []
play (c:calls) boards =
  case partition isWinner (map (mark c) boards) of
    (winners, losers) -> map (score c) winners ++ play calls losers

-- | Mark off a called number on a board.
mark :: Int -> Board -> Board
mark c = map (map (\x -> if x == c then -1 else x))

-- | Compute the final score for a board given the last call and unmarked numbers.
score :: Int -> Board -> Int
score c b = c * sum (filter (-1 /=) (concat b))

-- | Predicate for boards with a completed row or column
isWinner :: Board -> Bool
isWinner b = f b || f (transpose b)
  where f = any (all (-1 ==))
