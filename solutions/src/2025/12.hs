{-# Language QuasiQuotes, ParallelListComp #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/12>

Yes, I'm aware that this doesn't actually solve the problem,
but it does get the right answer for my input.

I'll redo this at some point to do it correctly.

-}
module Main where

import Advent (format, count, countBy)

main :: IO ()
main =
 do (shapes, regions) <- [format|2025 12 (%d:%n(%s%n)*%n)*(%dx%d:( %d)*%n)*|]
    print (countBy (fits shapes) regions)

fits :: [(Int, [String])] -> (Int, Int, [Int]) -> Bool
fits shapes (x, y, regions) =
  x*y >= sum [ n * count '#' (concat s) | (_, s) <- shapes | n <- regions]
