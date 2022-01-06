{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/19>

Elves sitting in a circle stealing presents.

-}
module Main where

import Advent (format)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

-- | >>> :main
-- 1830117
-- 1417887
main :: IO ()
main =
 do elves <- [format|2016 19 %u%n|]
    let ring = Seq.fromList [1..elves]
    print (part1 ring)
    print (part2 ring)

part1 :: Seq a -> a
part1 (x Seq.:<| Seq.Empty) = x
part1 (x Seq.:<| _ Seq.:<| xs) = part1 (xs Seq.|> x)

part2 :: Seq a -> a
part2 (x Seq.:<| Seq.Empty) = x
part2 (x Seq.:<| xs) = part2 (xs' Seq.|> x)
  where
    xs' = Seq.deleteAt (half (length xs)) xs
    half x = (x-1) `quot` 2
