{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main where

import Advent (countBy, format)
import Data.List (sort, nub)


main :: IO ()
main =
  do input <- [format|4 (%s& %n)*|]
     print (countBy allUnique input)
     print (countBy allUniqueModuloAnagrams input)


-- | Parse input as lines of words.
--
-- >>> parseInput "one two three\nfour five six\n"
-- [["one","two","three"],["four","five","six"]]
parseInput :: String -> [[String]]
parseInput = map words . lines


-- | Predicate that returns true when all elements in the list are unique
--
-- >>> allUnique (words "aa bb cc dd ee")
-- True
-- >>> allUnique (words "aa bb cc dd aa")
-- False
-- >>> allUnique (words "aa bb cc dd aaa")
-- True
allUnique :: Ord a => [a] -> Bool
allUnique x = x == nub x


-- | Predicate that returns true when all elements in the list are unique
-- when considering anagrams equal to each other.
--
-- >>> allUniqueModuloAnagrams (words "abcde fghij")
-- True
-- >>> allUniqueModuloAnagrams (words "abcde xyz ecdab")
-- False
-- >>> allUniqueModuloAnagrams (words "a ab abc abd abf abj")
-- True
-- >>> allUniqueModuloAnagrams (words "iiii oiii ooii oooi oooo")
-- True
-- >>> allUniqueModuloAnagrams (words "oiii ioii iioi iiio")
-- False
allUniqueModuloAnagrams :: Ord a => [[a]] -> Bool
allUniqueModuloAnagrams = allUnique . map sort
