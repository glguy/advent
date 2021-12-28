{-# Language QuasiQuotes, BlockArguments, LambdaCase #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/16>

We're given facts about a bunch of different /Sues/ and asked to
check which one matches what we know about the one true /Sue/.

-}
module Main where

import Advent.Format (format)

main :: IO ()
main =
 do input <- [format|16 (Sue %d: (%s: %d)&(, )%n)*|]
    print [i | (i, props) <- input, matchesClues1 props]
    print [i | (i, props) <- input, matchesClues2 props]

-- | Predicate for properties that match exactly.
matchesClues1 :: [(String,Int)] -> Bool
matchesClues1 = matcher (const (==))

-- | Predicate like 'matchesClues1' but with special cases for
-- /cats/, /trees/, /pomeranians/, and /goldfish/.
matchesClues2 :: [(String,Int)] -> Bool
matchesClues2 =
  matcher \case
    "cats"        -> (<)
    "trees"       -> (<)
    "pomeranians" -> (>)
    "goldfish"    -> (>)
    _             -> (==)

-- | Match a list of properties against the known hints.
matcher ::
  (String -> Int -> Int -> Bool) {- ^ comparison selector -} ->
  [(String,Int)] {- ^ list of properties -} ->
  Bool {- ^ properties match clues -}
matcher match = all \(prop, memory) ->
  match prop (clues prop) memory

-- | Returns the given hint value for each property.
clues :: String -> Int
clues "children"    = 3
clues "cats"        = 7
clues "samoyeds"    = 2
clues "pomeranians" = 3
clues "akitas"      = 0
clues "vizslas"     = 0
clues "goldfish"    = 5
clues "trees"       = 3
clues "cars"        = 2
clues "perfumes"    = 1
