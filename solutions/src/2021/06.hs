{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/6>

Multiplying fish! To make this problem tractable
track how many of each age of fish we have rather
than tracking all the individual ages of the fish.

-}
module Main (main) where

import Advent (counts, format, power)
import Data.Map (Map)
import Data.Map qualified as Map

-- | >>> :main
-- 376194
-- 1693022481538
main :: IO ()
main =
 do inp <- counts <$> [format|6 %u&,%n|]
    let bigFish = maximum (Map.keys inp)
    let oneStep = rule bigFish
    let nSteps = power (fmap . applyRule) oneStep
    print (sum (nSteps  80 `applyRule` inp))
    print (sum (nSteps 256 `applyRule` inp))

rule :: Int -> Map Int (Map Int Int)
rule n = counts <$> Map.fromList ((0, [6,8]) : [(i, [i-1]) | i <- [1..max n 8]])

applyRule :: (Ord a, Ord b) => Map a (Map b Int) -> Map a Int -> Map b Int
applyRule r m = Map.unionsWith (+) [(v *) <$> (r Map.! k) | (k,v) <- Map.toList m]
