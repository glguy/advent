{-# Language ImportQualifiedPost, QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/8>

-}
module Main (main) where

import Advent (format, stageTH)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

-- | Programs are expressed as control-flow graphs.
--
-- Nodes are instructions in the program.
--
-- Nodes are labeled with the accumulator-effect of executing that instruction.
--
-- Edges capture control flow between instructions.
--
-- Edges are labeled with the /cost/ of taking that edge. It costs @1@ to
-- take a control path generated from a toggled instruction.
data O = Onop | Ojmp | Oacc

stageTH

------------------------------------------------------------------------

-- |
-- >>> :main
-- 1200
-- 1023
main :: IO ()
main =
  do inp <- [format|2020 8 (@O (|%+)%d%n)*|]
     let pgm = IntMap.fromList (zip [0..] inp)
     print (snd (part1 pgm 0 0))
     print (part2 pgm 0 0)

part1 :: IntMap (O, Int) -> Int -> Int -> (Int,Int)
part1 pgm ip acc =
  let continue = part1 (IntMap.delete ip pgm) in
  case IntMap.lookup ip pgm of
    Nothing        -> (ip, acc)
    Just (Onop, _) -> continue (ip+1) acc
    Just (Oacc, n) -> continue (ip+1) (acc+n)
    Just (Ojmp, n) -> continue (ip+n) acc

part2 :: IntMap (O, Int) -> Int -> Int -> Int
part2 pgm ip acc =
  case pgm IntMap.! ip of
    (Onop, n) -> try (part1 pgm (ip+n) acc) (part2 pgm (ip+1) acc)
    (Ojmp, n) -> try (part1 pgm (ip+1) acc) (part2 pgm (ip+n) acc)
    (Oacc, n) -> part2 pgm (ip+1) (acc+n)
  where
    try (ip', acc') e
      | ip' == IntMap.size pgm = acc'
      | otherwise              = e
