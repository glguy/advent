{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/9>

This solution takes advantage of "Data.Sequence" providing efficient
access to both ends of a sequence. This gives us a amortized constant
time rotation operation and allows the solution to run quickly even at
the part 2 input size.

-}
{-# Language OverloadedStrings #-}
module Main (main) where

import Advent.Format (format)
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

-- | Print the answers to day 9
--
-- >>> :main
-- 422980
-- 3552041936
main :: IO ()
main =
  do [(players, marbles)] <- [format|2018 9 (%u players; last marble is worth %u points%n)*|]
     print (game players marbles)
     print (game players (100*marbles))

-- | Player the marble game, find maximum score
--
-- >>> game 10 1618
-- 8317
-- >>> game 13 7999
-- 146373
-- >>> game 17 1104
-- 2764
-- >>> game 21 6111
-- 54718
-- >>> game 30 5807
-- 37305
game :: Int {- ^ players -} -> Int {- ^ max marble -} -> Int {- ^ max score -}
game players marbles = go IntMap.empty (Seq.singleton 0) 1
  where
    go scores circle i

      -- game over, find winning score
      | i > marbles = maximum scores

      -- scoring marble, update current elf's score
      | isScoreMarble i =
          case rotate (-7) circle of
            Seq.Empty              -> error "game: empty circle"
            picked Seq.:<| circle' -> go scores' circle' (i+1)
              where
                scores' = IntMap.insertWith (+) (i `rem` players) (i + picked) scores

      -- normal turn, just add the marble
      | otherwise = go scores (i Seq.<| rotate 2 circle) (i+1)

-- | Rotate the elements of a sequence. Positive numbers index from the front.
-- Negative numbers index from the back. Indexes wrap around.
--
-- >>> rotate (-2) (Seq.fromList [0..10])
-- fromList [9,10,0,1,2,3,4,5,6,7,8]
-- >>> rotate 2 (Seq.fromList [0..10])
-- fromList [2,3,4,5,6,7,8,9,10,0,1]
-- >>> rotate 10 (Seq.fromList [0..2])
-- fromList [1,2,0]
rotate :: Int -> Seq a -> Seq a
rotate n xs
  | Seq.null xs = xs
  | otherwise   = case Seq.splitAt (n `mod` Seq.length xs) xs of
                    (l, r) -> r <> l

-- | Predicate for marbles that trigger a score event
isScoreMarble :: Int -> Bool
isScoreMarble i = i `rem` 23 == 0
