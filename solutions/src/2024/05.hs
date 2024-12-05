{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/5>

>>> :{
:main + "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13\n
75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"
:}
143
123

-}
module Main (main) where

import Advent (format)
import Data.Graph.Inductive (mkUGraph, topsort, UGr)
import Data.List (partition)

-- | >>> :main
-- 4996
-- 6311
main :: IO ()
main =
 do (ords, input) <- [format|2024 5 (%u%|%u%n)*%n(%u&,%n)*|]
    let (good,bad) = partition (valid ords) input
    print (sum (map middle (good)))
    print (sum (map (middle . fixup ords) bad))

-- | Put the pages back in order given the ordering constraints and page numbers.
fixup :: [(Int, Int)] -> [Int] -> [Int]
fixup ords pages = topsort (mkUGraph pages es :: UGr)
  where
    es = [(a,b) | (a,b) <- ords, a `elem` pages, b `elem` pages]

-- | Return the middle element of an odd-length list.
middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

-- | Predicate for pages orderings that satisfy the ordering constraints.
valid :: [(Int, Int)] -> [Int] -> Bool
valid ords x = all f ords
  where
    f (a,b) =
      case break (a==) x of
        (l, _:_) -> b `notElem` l
        _ -> True
