{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/15>

Solved using <https://en.wikipedia.org/wiki/Chinese_remainder_theorem>

-}
module Main where

import Advent (format)
import Advent.Chinese (chinese, toMod)

-- | >>> :main
-- Just 376777
-- Just 3903937
main :: IO ()
main =
 do input1 <- [format|2016 15 (Disc #%lu has %lu positions; at time=%lu, it is at position %lu.%n)*|]
    let input2 = input1 ++ [(fromIntegral (length input1) + 1, 11, 0, 0)]
    print (solve input1)
    print (solve input2)

-- | Given a list of discs, find the right time to push the button.
--
-- Example:
--
-- @
-- Disc #1 has 5 positions; at time=0, it is at position 4.
-- Disc #2 has 2 positions; at time=0, it is at position 1.
-- @
--
-- >>> solve [(1, 5, 0, 4), (2, 2, 0, 1)]
-- Just 5
solve ::
  [(Integer, Integer, Integer, Integer)] {- ^ disc location, disc size, initial time, initial rotations -} ->
  Maybe Integer {- ^ time to press button -}
solve discs = chinese [toMod (t-p-i) n | (i, n, t, p) <- discs] 
