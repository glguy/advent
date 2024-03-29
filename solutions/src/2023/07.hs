{-# Language QuasiQuotes, TransformListComp, ParallelListComp, ImportQualifiedPost #-}
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
import Data.List (sortOn, sortBy, elemIndex)
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Map qualified as Map

-- | Parse the input hands and print the answers to both parts.
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
strength1 hand = toRank (counts hand) ++ map val hand
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
strength2 hand = rank ++ map val hand
  where
    val x = fromJust (x `elemIndex` "J23456789TQKA")
    rank =
      case Map.updateLookupWithKey (\_ _ -> Nothing) 'J' (counts hand) of
          (Nothing   , sets) ->          toRank sets
          (Just wilds, sets) -> improve (toRank sets)
            where
              improve []       = [wilds]
              improve (x : xs) = x + wilds : xs

toRank :: Map Char Int -> [Int]
toRank = sortBy (flip compare) . toList
