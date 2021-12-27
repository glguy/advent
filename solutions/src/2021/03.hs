{-# Language ImportQualifiedPost, BlockArguments, LambdaCase, QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/3>

Select binary numbers using the most and least common bit
in each position.

-}
module Main (main) where

import Advent (count, format, fromDigits)
import Data.List (transpose)

-- | A bit
data B = B0 | B1 deriving (Read, Show, Eq, Ord)

-- | Bit complement
cmpl :: B -> B
cmpl B0 = B1
cmpl B1 = B0

-- | Interpret list of bits as a big-endian binary number
--
-- >>> fromBits [B1, B1, B0, B1]
-- 13
fromBits :: [B] -> Integer
fromBits = fromDigits 2 . map \case B0->0; B1->1

mempty -- make B available for reify in format

-- | >>> :main
-- 749376
-- 2372923
main :: IO ()
main =
 do inp <- [format|3 (@B*%n)*|]
    print (harness pick1 inp)
    print (harness pick2 inp)

-- | Use selection function to pick output bit by column
pick1 :: ([B] -> B) -> [[B]] -> [B]
pick1 sel xs = map sel (transpose xs)

-- | Use selection function to filter entries by each bit column
pick2 :: ([B] -> B) -> [[B]] -> [B]
pick2 _ [x] = x
pick2 sel xs = b : pick2 sel [ys | y:ys <- xs, b == y]
  where
    b = sel [y | y:_ <- xs]

-- | Given a function that requires a selection function run
-- it on the selection function picking the most and least frequent
-- values and then multiply those results together
harness :: (([B] -> B) -> [[B]] -> [B]) -> [[B]] -> Integer
harness k xs = fromBits (k rule xs) * fromBits (k (cmpl . rule) xs)

-- | Pick 1 when there are at least as many 1s as 0s
rule :: [B] -> B
rule xs
  | count B0 xs <= count B1 xs = B1
  | otherwise                  = B0
