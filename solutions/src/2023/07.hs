{-# Language QuasiQuotes, TransformListComp, ParallelListComp #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/7>

Sort the hands of a poker-like card game and compute the
resulting winnings.

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
module Main (main) where

import Advent (format, counts)
import Data.Foldable (toList)
import Data.List (sortOn, sort, elemIndex, nub)
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

-- | Compute the winnings after ordering the given hands by strength
-- and multiplying the bids by position in the ranked list.
winnings :: Ord a => (String -> a) -> [(String, Int)] -> Int
winnings strength input =
  sum [bid * rank | rank        <- [1..]
                  | (hand, bid) <- input, then sortOn by strength hand]

-- | Map a hand to a representative of its strength for part 1
--
-- >>> strength1 "2AAAA" < strength1 "33332"
-- True
--
-- >>> strength1 "77788" < strength1 "77888"
-- True
--
-- >>> strength1 "KTJJT" < strength1 "KK677"
-- True
--
-- >>> strength1 "T55J5" < strength1 "QQQJA"
-- True
strength1 :: String -> [Int]
strength1 hand = category hand : map val hand
  where
    val x = fromJust (x `elemIndex` "23456789TJQKA")

-- | Map a hand to a representative of its strength for part 2.
-- This version treats @J@ as a wildcard of low individual value.
--
-- >>> strength2 "JKKK2" < strength2 "QQQQ2"
-- True
--
-- >>> sortOn strength2 ["T55J5", "KTJJT", "QQQJA"]
-- ["T55J5","QQQJA","KTJJT"]
strength2 :: String -> [Int]
strength2 hand =
  maximum
    [ category (map rpl hand)
    | alt <- nub hand
    , let rpl x = if x == 'J' then alt else x
    ] : map val hand
  where
    val x = fromJust (x `elemIndex` "J23456789TQKA")

-- | Map a hand to an integer representing its set size.
category :: String -> Int
category hand =
  case sort (toList (counts hand)) of
    [5]         -> 6
    [1,4]       -> 5
    [2,3]       -> 4
    [1,1,3]     -> 3
    [1,2,2]     -> 2
    [1,1,1,2]   -> 1
    [1,1,1,1,1] -> 0
    _           -> error "bad hand"
