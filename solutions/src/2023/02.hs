{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/2>

-}
module Main where

import Data.Map qualified as Map

import Advent (format)

-- |
--
-- >>> :main
-- 2169
-- 60948
main :: IO ()
main =
 do input <- [format|2023 2 (Game %d: (%d %s)&(, )&(; )%n)*|]
    print (sum [i | (i, gameSets) <- input, all (all part1) gameSets])
    print (sum
      [product (Map.fromListWith max [(s,n) | (n,s) <- concat gameSets])
      | (_, gameSets) <- input])

part1 :: (Int, String) -> Bool
part1 (v,k) =
  case lookup k [("red", 12), ("green", 13), ("blue", 14)] of
    Nothing -> False
    Just n  -> n >= v
