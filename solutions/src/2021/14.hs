{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/14>

Build a huge polymer chain and compute how many of
each element it contains.

This problem requires memoization as the size of the
resulting polymer would be humongous!

-}
module Main (main) where

import Advent (format, power, counts)
import Data.Map (Map)
import Data.Map.Strict qualified as Map

-- | >>> :main
-- 2068
-- 2158894777814
main :: IO ()
main =
 do (seed, table) <- [format|14 %s%n%n(%c%c -> %c%n)*|]
    let rule = tableToRule table
    print (solve rule 10 seed)
    print (solve rule 40 seed)

solve :: Ord a => Map (a,a) (Map (a,a) Int) -> Integer -> [a] -> Int
solve rule n seed = maximum occ - minimum occ
  where
    ruleN = power (fmap . applyRule) rule n

    start = counts (zip seed (tail seed))

    occ = Map.insertWith (+) (head seed) 1
        $ Map.mapKeysWith (+) snd
        $ applyRule ruleN start

-- | Generate a replacement rule map from a list of input productions
--
-- >>> tableToRule [('L','R','M')] -- LR -> M
-- fromList [(('L','R'),fromList [(('L','M'),1),(('M','R'),1)])]
tableToRule :: Ord a => [(a,a,a)] -> Map (a,a) (Map (a,a) Int)
tableToRule xs = Map.fromList [((l,r), counts [(l,m), (m,r)]) | (l,r,m) <- xs]

-- | Apply a replacement rule to a map of counts.
--
-- >>> :set -XOverloadedLists
-- >>> applyRule [('a', [('b',1),('c',2)]),('z',[('y',1)])] [('a',10)]
-- fromList [('b',10),('c',20)]
applyRule :: (Ord a, Ord b) => Map a (Map b Int) -> Map a Int -> Map b Int
applyRule r m = Map.unionsWith (+) [(v *) <$> r Map.! k | (k,v) <- Map.toList m]
