{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/18>

Run Rule 90, a cellular automaton, for a few generations and count how many
cells are turned on.

<https://en.wikipedia.org/wiki/Rule_90>

-}
module Main (main) where

import Advent (count, format)

-- | >>> :main
-- 2005
-- 20008491
main :: IO ()
main =
 do input <- [format|2016 18 %s%n|]
    print (solve input     40)
    print (solve input 400000)

-- | Given a seed and number of generations, count the safe tiles in the map.
solve ::
  String {- ^ seed -} ->
  Int {- ^ generations -} ->
  Int {- ^ total safe tiles -}
solve input n = count '.' (concat (take n (iterate next input)))

-- | Update logic for rule 90 based on previous cell values.
rule90 :: Char {- ^ left -} -> Char {- ^ center -} -> Char {- ^ right -} -> Char
rule90 x _ y
  | x /= y    = '^'
  | otherwise = '.'

-- | Compute the next generation.
next :: String -> String
next (x:xs) = go '.' x xs
  where
    go a b [] = [rule90 a b '.']
    go a b (c:cs) = rule90 a b c : go b c cs
