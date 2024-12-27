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
import Data.List (tails)

-- | >>> :main
-- 2618
main :: IO ()
main =
 do input <- [format|2024 25 (%s%n)*&%n|]
    print (length [() | x : ys <- tails (map concat input), y <- ys, and (zipWith ok x y)])

ok :: Char -> Char -> Bool
ok '#' '#' = False
ok _   _   = True