{-# Language QuasiQuotes, TransformListComp, ParallelListComp #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/7>

>>> :{
:main +
"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"
:}
6440
5905

-}
module Main where

import Advent (format, counts)
import Data.List (sortOn, sort, elemIndex)
import Data.Foldable (toList)
import Data.Maybe (fromJust)

-- |
--
-- >>> :main
-- 248422077
-- 249817836
main :: IO ()
main =
 do input <- [format|2023 7 (%s %d%n)*|]
    print (winnings strength1 input)
    print (winnings strength2 input)

winnings :: Ord a => (String -> a) -> [(String, Int)] -> Int
winnings strength input =
  sum [bid * rank | rank        <- [1..]
                  | (hand, bid) <- input, then sortOn by strength hand]

strength1 :: String -> [Int]
strength1 hand = category hand : map val hand
  where
    val x = fromJust (x `elemIndex` "23456789TJQKA")

strength2 :: String -> [Int]
strength2 a = maximum [category (map rpl a) : map val a
                      | alt <- "23456789TQKA"
                      , let rpl x = if x == 'J' then alt else x
                      ]
  where
    val x = fromJust (x `elemIndex` "J23456789TQKA")

category :: String -> Int
category m =
  case sort (toList (counts m)) of
    [5]         -> 6
    [1,4]       -> 5
    [2,3]       -> 4
    [1,1,3]     -> 3
    [1,2,2]     -> 2
    [1,1,1,2]   -> 1
    [1,1,1,1,1] -> 0
    _           -> error "bad hand"
