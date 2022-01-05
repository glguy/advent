{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/10>

-}
module Main (main) where

import Advent (count)
import Advent.Format (format)
import Data.List (sort)

-- |
-- >>> :main
-- 1998
-- 347250213298688
main :: IO ()
main =
  do adapters <- [format|2020 2020 10 (%u%n)*|]
     let socket = 0
     let device = maximum adapters + 3

     let jolts = sort (socket : device : adapters)
     let diffs = zipWith (-) (tail jolts) jolts
     print (count 3 diffs * count 1 diffs)

     let part2 (1:ds) x y z = part2 ds y z (z+y+x)
         part2 (2:ds) _ y z = part2 ds z 0 (z+y) -- unused in normal input
         part2 (3:ds) _ _ z = part2 ds 0 0 z
         part2 []     _ _ z = z
         part2 _      _ _ _ = error "unexpected gap"
     print (part2 diffs 0 0 1 :: Integer)
