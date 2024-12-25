{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/25>

-}
module Main (main) where

import Advent (format)
import Advent.Coord (coordLines)
import Data.List (tails)
import Data.Set qualified as Set

-- | >>> :main
-- 2618
main :: IO ()
main =
 do input <- [format|2024 25 (%s%n)*&%n|]
    let grids = [Set.fromList [c | (c, '#') <- coordLines xs] | xs <- input]
    print (length [()
       | x : xs <- tails grids
       , y <- xs
       , Set.null (Set.intersection x y)
       ])