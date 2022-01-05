{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/4>

-}
module Main where

import Advent (countBy, format)
import Data.List (sort, nub)


main :: IO ()
main =
  do input <- [format|2017 4 (%s& %n)*|]
     print (countBy allUnique input)
     print (countBy allUniqueModuloAnagrams input)


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
