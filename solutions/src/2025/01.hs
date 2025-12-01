{-# Language QuasiQuotes, TemplateHaskell, ViewPatterns #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/1>

>>> :main + "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n"
3
6

-}
module Main (main) where

import Advent (count, format, stageTH)
import Data.List (mapAccumL)

-- | Direction of the turn: left (negative) or right (positive)
data D = DL | DR deriving Show

stageTH

-- | >>> :main
-- 992
-- 6133
main :: IO ()
main =
 do input <- [format|2025 1 (@D%d%n)*|]
    let (stops, zeros) = unzip (sim 50 input)
    print (count 0 stops)
    print (sum zeros)

-- | Simulate a list of dial turns given a starting location to produce
-- the list of intermediate dial locations as well as the number of times
-- zero was passed each turn.
sim ::
  Int          {- ^ current location -} ->
  [(D, Int)]   {- ^ list of turns -} ->
  [(Int, Int)] {- ^ trace of stop locations and zero passes -}
sim _   []             = []
sim loc ((DR, n) : xs) = (loc', zeros) : sim loc' xs where (zeros,        loc') = (    loc + n) `divMod` 100
sim loc ((DL, n) : xs) = (loc', zeros) : sim loc' xs where (zeros, neg -> loc') = (neg loc + n) `divMod` 100

-- | Negated dial location used to be able to treat left turns a positive turns on a negated dial.
-- 
-- >>> map neg [0,1,2,98,99]
-- [0,99,98,2,1]
neg :: Int -> Int
neg x = (-x) `mod` 100
