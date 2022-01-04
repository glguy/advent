{-# Language ViewPatterns #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/24>

These programs were composed of 14 nearly-identical program chunks
each of which varies in 3 parameters. Some of these chunks will
always increase the @z@ register and others have the potential to
decrease @z@. Successfully validating an input will require all
blocks with decrease potential to actually decrease. The 'pick'
implementation will only choose a parameter that would decrease
in one of these cases.

-}
module Main (main) where

import Advent (getInputLines, chunks, fromDigits, scanlM)
import Text.Read (readMaybe)

-- | >>> :main
-- 49917929934999
-- 11911316711816
main :: IO ()
main =
 do pgm <- map extract . chunks 18 . map words <$> getInputLines 24
    print (solve [9,8..1] pgm)
    print (solve [1,2..9] pgm)

-- | Compute the input string that satisfies the given program.
solve ::
  [Int]             {- ^ input digit guesses in order of preference -} ->
  [(Int, Int, Int)] {- ^ program blocks -} ->
  Int               {- ^ first valid input -}
solve guesses pgm =
  head [fromDigits 10 input | (input, 0) <- scanlM (pick guesses) 0 pgm]

-- | Compute the possible input choices and resulting z from each choice.
pick ::
  [Int]           {- ^ input digit guesses in order of preference -} ->
  (Int, Int, Int) {- ^ block parameters -} ->
  Int             {- ^ starting z value -} ->
  [(Int, Int)]    {- ^ selected input value and resulting output z -}
pick guesses (a,b,c) z =
  [ (i, impl a b c i z)
  | i <- if a == 26 then [w | let w = z`mod`26 + b, 1 <= w, w <= 9]
                    else guesses
  ]

-- | Extract the variable parameters from a single block. These parameters
-- can be passed into 'impl' to compute this effect's block on @z@ given
-- an input digit. Programs are comprised of one of these blocks per each
-- digit of the input.
extract :: [[String]] -> (Int, Int, Int)
extract [
  ["inp", "w"      ],
  ["mul", "x", "0" ],
  ["add", "x", "z" ],
  ["mod", "x", "26"],
  ["div", "z", readMaybe -> Just a],
  ["add", "x", readMaybe -> Just b],
  ["eql", "x", "w" ],
  ["eql", "x", "0" ],
  ["mul", "y", "0" ],
  ["add", "y", "25"],
  ["mul", "y", "x" ],
  ["add", "y", "1" ],
  ["mul", "z", "y" ],
  ["mul", "y", "0" ],
  ["add", "y", "w" ],
  ["add", "y", readMaybe -> Just c],
  ["mul", "y", "x" ],
  ["add", "z", "y" ]] =
  (a, b, c)
extract x = error (show x)

-- | Manually compiled behavior of an input block with parameters determined
-- by 'extract'.
impl ::
  Int {- ^ first parameter -} ->
  Int {- ^ second parameter -} ->
  Int {- ^ third parameter -} ->
  Int {- ^ input digit -} ->
  Int {- ^ z register -} ->
  Int {- ^ z register -}
impl a b c w z
  | z`mod`26 + b == w = z `div` a
  | otherwise         = z `div` a * 26 + w + c
