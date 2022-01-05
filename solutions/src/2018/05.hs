{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/5>

-}
module Main (main) where

import Advent.Format (format)
import Data.Char (toLower, toUpper)
import Data.List (nub)

-- | Print the answers to day 5
--
-- >>> :main
-- 9370
-- 6390
main :: IO ()
main =
  do inp <- [format|2018 5 %s%n|]
     print (part1 inp)
     print (part2 inp)

-- | Collapse the polymer using the lowercase/uppercase matching rule.
--
-- >>> simplify "dabAcCaCBAcCcaDA"
-- "dabCBAcaDA"
simplify :: String -> String
simplify = foldr step ""
  where
    -- Invariant: list argument to step is always completely reduced
    step x (y:ys) | match x y = ys
    step x ys                 = x : ys

-- | Match characters where one is the lowercase version of the other.
--
-- >>> match 'a' 'A'
-- True
-- >>> match 'A' 'a'
-- True
-- >>> match 'a' 'a'
-- False
match :: Char -> Char -> Bool
match x y = x /= y && toUpper x == toUpper y

-- | Compute the length of the collapsed polymer.
--
-- >>> part1 "dabAcCaCBAcCcaDA"
-- 10
part1 :: String -> Int
part1 = length . simplify


-- | Find the minimum length a string can reduce to when we're allowed
-- to remove any one pair of characters.
--
-- >>> part2 "dabAcCaCBAcCcaDA"
-- 4
part2 :: String -> Int
part2 str = minimum lengths
  where
    str'       = simplify str
    candidates = nub (map toLower str')
    isOk bad x = bad /= toLower x
    lengths    = [part1 (filter (isOk bad) str') | bad <- candidates]
