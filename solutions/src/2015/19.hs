{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/19>

We get a bunch of reaction rules and a final string. Our goal is to find
the smallest application of rules to produce that final string from an
initial empty string.

This solution uses dynamic programming to build the answer up out of all
the most efficient ways to build the substrings of the final string out
of individual starting atoms.

-}
module Main (main) where

import Advent (minimumMaybe, counts, format)
import Data.Array (Ix(range), Array, (!), array, bounds, listArray)
import Data.Char (isLower)
import Data.List (groupBy, inits, tails)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set qualified as Set

newtype Atom = Atom String
  deriving (Eq,Ord)

main :: IO ()
main =
  do (rules_, input_) <- [format|19 (%s => %s%n)*%n%s%n|]
     let rules = Map.fromListWith (++) [(Atom a, [parseMolecule b]) | (a,b) <- rules_]
     let input = parseMolecule input_
     print (length (counts (oneStep rules input)))
     print (minRulesNeeded rules input (Atom "e"))

oneStep :: Ord a => Map a [[a]] -> [a] -> [[a]]
oneStep rules input =
  [ xs ++ z ++ ys
    | (xs,y:ys) <- zip (inits input) (tails input)
    , z <- Map.findWithDefault [] y rules
  ]

-- | Add empty elements to the map so that every @a@ that occurs in
-- the values of the map also occurs in the keys.
extendRules :: Ord a => Map a [[a]] -> Map a [[a]]
extendRules rules = Map.unionWith (++) extraRules rules
  where
  extraRules = Map.fromSet (const [])
             $ Set.fromList (concat (concat (Map.elems rules)))

-- |
-- > parseMolecule "AbCdEF"
-- [Atom "Ab", Atom "Cd", Atom "E", Atom "F"]
parseMolecule :: String -> [Atom]
parseMolecule = map Atom . groupBy (\_ y -> isLower y)

-- | Given a map of rewrite rules rewriting the keys to any of the
-- alternatives, return the minimum number of rewrites needed to rewrite
-- the start symbol into the input.
minRulesNeeded ::
  Ord a =>
  Map a [[a]] {- ^ rules, sum of products -} ->
  [a]         {- ^ input                  -} ->
  a           {- ^ start state            -} ->
  Maybe Int
minRulesNeeded rules input start = minRulesNeededInt ruleArr inputArr (toInt start)
  where
  rules'  = extendRules rules
  toInt x = Map.findIndex x rules'

  numRules = Map.size rules'
  numInput = length input
  inputArr = listArray (0,numInput-1) (map toInt input)
  ruleArr  = listArray (0,numRules-1) (Map.elems (fmap (map (map toInt)) rules'))

-- | Given an array of inputs determine how many rule applications
-- are required to transform the start state into the input.
--
-- This solution uses dynamic programming. The solutions are memoized
-- as about how many steps, if any, each substring of the input takes to
-- match each of the symbols in the alphabet.
minRulesNeededInt ::
  Ix i =>
  Array i [[i]] {- ^ rules, sum of products -} ->
  Array Int i   {- ^ input -} ->
  i             {- ^ start -} ->
  Maybe Int     {- ^ minimum rules needed -}
minRulesNeededInt rules input = cost inputLo inputHi
  where
  (inputLo,inputHi) = bounds input
  (rulesLo,rulesHi) = bounds rules
  costBounds        = ((inputLo,inputLo,rulesLo)
                      ,(inputHi,inputHi,rulesHi))

  cost start end rule = costArray ! (start,end,rule)

  costArray = generate costBounds cost'

  cost' (start,end,ruleIx)
    | start == end, input ! start == ruleIx = Just 0
    | otherwise = fmap succ
                $ minimumMaybe
                $ mapMaybe (nonTerm start end)
                $ rules ! ruleIx

  nonTerm start end rhs =
    case rhs of
     []   -> Nothing
     [x]  -> cost start end x
     x:xs -> minimumMaybe
               [ cost1 + cost2
               | mid   <- [start .. end - length xs]
               , cost1 <- maybeToList (cost start mid x)
               , cost2 <- maybeToList (nonTerm (succ mid) end xs)
               ]

-- | Generate an array given the bounds an a function from indexes to elements.
generate :: Ix i => (i,i) -> (i -> e) -> Array i e
generate bnd f = array bnd [ (i, f i) | i <- range bnd ]
