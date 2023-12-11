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
import Data.List
import Data.Map qualified as Map
import Advent
import Advent.Coord (coordLines, Coord(C))

-- |
--
-- >>> :main
-- 9965032
-- 550358864332
main :: IO ()
main =
 do input <- getInputLines 2023 11
    let galaxies = [c | (c,'#') <- coordLines input]
    let rows = Map.assocs (counts [y | C y _ <- galaxies])
    let cols = Map.assocs (counts [x | C _ x <- galaxies])

    let solve1 n xs =
          sum
          [ (fst (head r) - fst (last l) - 1) * n * (sum (map snd l) * sum (map snd r))
          | (l,r) <- zip (inits xs) (tails xs), not (null l), not (null r)]
          + sum
          [ sum (map snd l) * sum (map snd r)
          | (l,r) <- zip (inits xs) (tails xs)]

    let solve n = solve1 n rows + solve1 n cols

    print (solve 2)
    print (solve 1_000_000)
