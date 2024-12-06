{-# Language QuasiQuotes, ViewPatterns #-}
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
import Algebra.Graph.AdjacencyIntMap (edges, induce)
import Algebra.Graph.AdjacencyIntMap.Algorithm (isTopSortOf, topSort)
import Data.List (partition)

-- | >>> :main
-- 4996
-- 6311
main :: IO ()
main =
 do (ords, pagess) <- [format|2024 5 (%u%|%u%n)*%n(%u&,%n)*|]
    let graph       = edges ords
        inputs      = [(pages, induce (`elem` pages) graph) | pages <- pagess]
        (good, bad) = partition (uncurry isTopSortOf) inputs
    print (sum (map (middle . fst) good))
    print (sum (map (either undefined middle . topSort . snd) bad))

-- | Return the middle element of an odd-length list.
middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)
