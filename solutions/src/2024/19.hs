{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost, ParallelListComp, BangPatterns #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/19>

>>> :{
:main + "r, wr, b, g, bwu, rb, gb, br
\&
brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
"
:}
6
16

-}
module Main (main) where

import Advent (format, countBy)
import Data.Array (Array, (!), listArray)
import Data.List (tails)
import Data.Map (Map)
import Data.Map qualified as Map

-- | >>> :main
-- 319
-- 692575723305545
main :: IO ()
main =
 do (available, desired) <- [format|2024 19 %s&(, )%n%n(%s%n)*|]
    let ways = map (designWays (foldMap toTrie available)) desired
    print (countBy (> 0) ways)
    print (sum ways)

-- | Compute the number of ways a design can be created using a trie
-- of available patterns.
designWays :: Ord a => Trie a -> [a] -> Int
designWays t str = memo ! 0
  where
    n = length str
    memo :: Array Int Int
    memo = listArray (0, n)
           [ if i == n then 1 else sum [memo ! j | j <- matches t i suffix]
           | i      <- [0 .. n]
           | suffix <- tails str]

-- | Efficient structure for finding all of the prefixes of a list that match.
data Trie a = Node !Bool (Map a (Trie a))

-- | Construct a 'Trie' that matches exactly one list.
toTrie :: Ord a => [a] -> Trie a
toTrie = foldr cons (Node True Map.empty)

-- | Extend a 'Trie' to match with a prefix element.
cons :: a -> Trie a -> Trie a
cons x t = Node False (Map.singleton x t)

-- | Given a starting index find all the ending indexes for
-- suffixes that remain after matching a string in the 'Trie'.
--
-- >>> matches (toTrie "pre" <> toTrie "pref") 0 "prefix"
-- [3,4]
matches :: Ord a => Trie a -> Int -> [a] -> [Int]
matches (Node b xs) !n yys =
  [n | b] ++
  case yys of
    y:ys | Just t <- Map.lookup y xs -> matches t (n+1) ys
    _ -> []

-- | '<>' constructs the union of two 'Trie's.
instance Ord a => Semigroup (Trie a) where
  Node x xs <> Node y ys = Node (x || y) (Map.unionWith (<>) xs ys)

-- | 'mempty' is a 'Trie' that matches nothing.
instance Ord a => Monoid (Trie a) where
  mempty = Node False Map.empty
