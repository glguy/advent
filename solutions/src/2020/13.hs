{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/13>

-}
module Main (main) where

import Advent.Chinese (toMod, chinese)
import Advent.Format (format)
import Data.Foldable (traverse_)
import Data.List (foldl1')

-- |
-- >>> :main
-- 3215
-- 1001569619313439
main :: IO ()
main =
  do (t,rawBusses) <- [format|13 %lu%n(x|%lu)&,%n|]
     let busses = [(i,b) | (i, Just b) <- zip [0..] rawBusses]
     print (part1 t (map snd busses))
     traverse_ print (part2 busses)

part1 :: Integer -> [Integer] -> Integer
part1 t busses = uncurry (*) (minimum [((-t)`mod`b, b) | b <- busses])

part2 :: [(Integer, Integer)] -> Maybe Integer
part2 busses = chinese [toMod (-x) y | (x,y) <- busses]
