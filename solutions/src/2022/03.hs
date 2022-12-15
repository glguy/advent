{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/3>

>>> :{
  :main +
    "vJrwpWtwJgWrhcsFMMfFFhFp\n\
    \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
    \PmmdzqPrVvPwwTWBwg\n\
    \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
    \ttgJtRGJQctTZtZT\n\
    \CrZsJsPPZsGzwwsLwLmpwMDw\n"
:}
157
70

-}
module Main where

import Data.Char (isLower, ord)
import Data.List (foldl1')
import Data.Set qualified as Set

import Advent (format, chunks)

-- |
-- >>> :main
-- 7917
-- 2585
main :: IO ()
main =
 do input <- [format|2022 3 (%s%n)*|]
    print (sum (map (score . halves) input))
    print (sum (map score (chunks 3 input)))

halves :: String -> [String]
halves xs = chunks (length xs `div` 2) xs

score :: [String] -> Int
score = priority . minimum . foldl1' Set.intersection . map Set.fromList

priority :: Char -> Int
priority x
  | isLower x = ord x - ord 'a' + 1
  | otherwise = ord x - ord 'A' + 27
