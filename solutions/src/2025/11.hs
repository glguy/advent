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
import Advent.Memo (memo, memo3)
import Data.Map qualified as Map

-- | >>> :main
-- 585
-- 349322478796032
main :: IO ()
main =
 do input <- [format|2025 11 (%s:( %s)*%n)*|]
    let tab = Map.fromList input

    let part1 = memo \loc ->
          if loc == "out" then 1
          else sum [part1 dst | dst <- Map.findWithDefault [] loc tab] 
    print (part1 "you")

    let part2 = memo3 \loc dac fft ->
          if loc == "out" then
            if dac && fft then 1 else 0
          else sum [part2 dst dac' fft'
                    | let dac' = dac || loc == "dac"
                    , let fft' = fft || loc == "fft"
                    , dst <- Map.findWithDefault [] loc tab
                    ]
    print (part2 "svr" False False)
