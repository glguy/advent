{-# Language BlockArguments, ParallelListComp #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/3>

>>> :{
:main +
"987654321111111
811111111111119
234234234234278
818181911112111
"
:}
357
3121910778619

-}
module Main (main) where

import Advent ( getInputLines )
import Data.Char ( digitToInt )

-- | >>> :main
-- 17445
-- 173229689350551
main :: IO ()
main =
 do input <- getInputLines 2025 3
    let parts = map (solveLine . map digitToInt) input
    print (sum [p !!  1 | p <- parts])
    print (sum [p !! 11 | p <- parts])

-- | Find the largest value that can be created by selecting
-- each number of digits from the list starting at 1.
solveLine ::
  [Int]   {- ^ row of digits -} ->
  [Int]   {- ^ largest value that can be selected for each size -}
solveLine = foldl addDigit (repeat 0)

-- | Given a list of best values so far when taking
-- [1, 2 .. n] digits produce a new best list
-- considering this new digit.
addDigit :: [Int] -> Int -> [Int]
addDigit prev d =
  [ max a (b * 10 + d)
    | a <- prev      -- keep the old best value, or
    | b <- 0 : prev] -- add this digit to the best value one smaller
