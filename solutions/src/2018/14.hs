{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/14>
-}
module Main (main) where

import Advent.Format (format)
import Data.Char (digitToInt)
import Data.List (findIndex, isPrefixOf, tails, foldl')
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

-- | Print the answers to day 14
--
-- >>> :main
-- 7116398711
-- 20316365
main :: IO ()
main =
  do input <- [format|2018 14 %u%n|]
     putStrLn (part1 input)
     print (part2 input)

-- | Find the next 10 recipies after dropping the first @input@ recipies.
--
-- >>> part1 5
-- "0124515891"
-- >>> part1 18
-- "9251071085"
-- >>> part1 2018
-- "5941429882"
part1 :: Int -> String
part1 input = concatMap show (take 10 (drop input entries))

-- | Find the index of the first occurrence of a list of recipies
--
-- >>> part2 51589
-- 9
-- >>> part2 92510
-- 18
-- >>> part2 59414
-- 2018
part2 :: Int -> Int
part2 input = fromJust (findIndex (toDigits input `isPrefixOf`) (tails entries))

data Cooks = Cooks !Int !Int !(Seq Int)
  deriving Show

-- | Infinite list of recipies created by the elves.
--
-- >>> take 12 entries
-- [3,7,1,0,1,0,1,2,4,5,1,5]
entries :: [Int]
entries = 3 : 7 : generate (Cooks 0 1 (Seq.fromList [3,7]))

-- | Turn an integer into a list of its digits
--
-- >>> toDigits 0
-- [0]
-- >>> toDigits 5
-- [5]
-- >>> toDigits 15
-- [1,5]
toDigits :: Int -> [Int]
toDigits = map digitToInt . show

-- | Generate the infinite list of recipies created by the elves.
generate :: Cooks -> [Int]
generate (Cooks elf1 elf2 recipies) =
  news ++ generate (Cooks elf1' elf2' recipies')
  where
    elf1r = Seq.index recipies elf1
    elf2r = Seq.index recipies elf2

    news = toDigits (elf1r + elf2r)
    recipies' = foldl' (Seq.|>) recipies news

    elf1' = nextPos elf1 elf1r
    elf2' = nextPos elf2 elf2r
    nextPos i r = (i + r + 1) `rem` Seq.length recipies'
