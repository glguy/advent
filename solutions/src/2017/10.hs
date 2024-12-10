{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/10>

Day 10 poses a convoluted knot-tying algorithm to implement.

-}
module Main where

import Advent           (getInputLines)
import Advent.KnotHash  (knotHash, tieKnots)
import Data.Char        (ord)
import Data.List.Split  (splitOn)
import Numeric.Natural  (Natural)
import Text.Printf      (printf)

-- | Print the solution to both parts of Day 10. Input file is configurable
-- via the command-line.
--
-- >>> :main
-- 23874
-- e1a65bfb5a5ce396025fab5528c25a87
main :: IO ()
main =
  do [inputLine] <- getInputLines 2017 10
     putStrLn (part1 256 inputLine)
     putStrLn (part2 inputLine)

-- | Compute the product of the first two elements after performing
-- the knot-tying ritual using the lengths given as inputs.
--
-- >>> :set -XDataKinds
-- >>> part1 5 "3,4,1,5"
-- "12"
part1 ::
  Natural ->
  String {- ^ input string -} ->
  String {- ^ output hash  -}
part1 n = show . product . map toInteger . take 2 . tieKnots n . part1Input

-- | Given a rope size and an input string, compute the resulting hash.
--
-- >>> part2 ""
-- "a2582a3a0e66e6e86e3812dcb672a272"
-- >>> part2 "AoC 2017"
-- "33efeb34ea91902bb2f59c9920caa6cd"
-- >>> part2 "1,2,3"
-- "3efbe78a8d82f29979031a4aa0b16a9d"
-- >>> part2 "1,2,4"
-- "63960835bcdc130f0b66d7ff4f6a5a8e"
part2 ::
  String {- ^ input string -} ->
  String {- ^ output hash  -}
part2 = printf "%032x" . knotHash

-- | Transform the input string according to the part 1 rule to
-- produce the list of knot lengths required.
--
-- >>> part1Input "1,2,3"
-- [1,2,3]
part1Input ::
  String {- ^ input string -} ->
  [Int]  {- ^ rope lengths -}
part1Input = map read . splitOn ","

-- | Transform the input string according to the part 2 rule to
-- produce the list of knot lengths required.
--
-- >>> part2Input "1,2,3"
-- [49,44,50,44,51,17,31,73,47,23]
part2Input ::
  String {- ^ input string -} ->
  [Int]  {- ^ rope lengths -}
part2Input str = map ord str ++ [17, 31, 73, 47, 23]
