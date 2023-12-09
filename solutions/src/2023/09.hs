{-# Language QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/9>

Extend an 

>>> :{
:main +
"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"
:}
114
2

-}
module Main where

import Advent (format)

-- | Parse the input and print out answers to both parts.
--
-- >>> :main
-- 1762065988
-- 1066
main :: IO ()
main =
 do input <- [format|2023 9 (%d& %n)*|]
    print (sum (map nextInSequence input))
    print (sum (map (nextInSequence . reverse) input))

nextInSequence :: [Int] -> Int
nextInSequence = sum . map last . takeWhile (any (0 /=)) . iterate differences

differences :: [Int] -> [Int]
differences xs = zipWith (-) (tail xs) xs