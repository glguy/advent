{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/19>

-}
module Main where

import Advent (format, countBy)
import Advent.Memo (memo)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import Data.Map qualified as Map

-- | >>> :main
-- 319
-- 692575723305545
main :: IO ()
main =
 do (available, desired) <- [format|2024 19 %s&(, )%n%n(%s%n)*|]
    let t = foldMap toTrie available
    let ways = [
          possible 0
          | x <- desired
          , let n = length x
          , let possible = memo \i ->
                  if n == i
                    then 1 :: Int
                    else sum (map possible (matches t i (drop i x)))
          ]
    print (countBy (> 0) ways)
    print (sum ways)

data Trie = Node !Bool (Map Char Trie)

toTrie :: String -> Trie
toTrie ""     = Node True Map.empty
toTrie (x:xs) = Node False (Map.singleton x (toTrie xs))

matches :: Trie -> Int -> String -> [Int]
matches (Node b xs) n yys =
  [n | b] ++
  case yys of
    y:ys | Just t <- Map.lookup y xs -> matches t (n+1) ys
    _ -> []

instance Semigroup Trie where
  Node x xs <> Node y ys = Node (x || y) (Map.unionWith (<>) xs ys)

instance Monoid Trie where
  mempty = Node False Map.empty
