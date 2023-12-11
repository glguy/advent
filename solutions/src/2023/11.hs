{-# Language QuasiQuotes, NumericUnderscores, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/11>

-}
module Main where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.List ( findIndices, tails, transpose )

import Advent (getInputLines)
import Advent.Coord (coordLines, Coord(C))

-- |
--
-- >>> :main
-- 9965032
-- 550358864332
main :: IO ()
main =
 do input <- getInputLines 2023 11
    let galaxies = Set.fromList [c | (c,'#') <- coordLines input]
    let bigrows = Set.fromList (findIndices (all ('.'==)) input)
    let bigcols = Set.fromList (findIndices (all ('.'==)) (transpose input))
    let solve n = sum [width n bigrows y1 y2 + width n bigcols x1 x2
                      | (C y1 x1, C y2 x2) <- pairs (Set.toList galaxies)]
    print (solve 2)
    print (solve 1_000_000)

pairs :: [a] -> [(a, a)]
pairs xs = [(x,y) | x:ys <- tails xs, y <- ys]

width :: Int -> Set Int -> Int -> Int -> Int
width expansion set x y = (expansion-1) * expands + hi - lo
  where
    lo = min x y
    hi = max x y
    expands = length (fst (Set.split hi (snd (Set.split lo set))))