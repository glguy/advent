{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/11>

-}
module Main where

import Advent (format)
import Advent.Memo (memo2)
import Data.Map qualified as Map

-- | >>> :main
-- 585
-- 349322478796032
main :: IO ()
main =
 do input <- [format|2025 11 (%s:( %s)*%n)*|]
    let tab = Map.fromList input

    let ways = memo2 \src dst ->
          if src == dst then 1
          else sum [ways nxt dst | nxt <- Map.findWithDefault [] src tab]

    print (ways "you" "out")
    print (ways "svr" "fft" * ways "fft" "dac" * ways "dac" "out" +
           ways "svr" "dac" * ways "dac" "fft" * ways "fft" "out")