{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/1>

Find entries in an /expense report/ that sum up to 2020 and return their
product.

-}
module Main where

import Advent.Format (format)
import Data.List (sort)

-- | >>> :main
-- 494475
-- 267520550
main :: IO ()
main =
  do input <- [format|2020 1 (%u%n)*|]
     print (solve input 2)
     print (solve input 3)

-- | Given a list of integer inputs, find the product of @count@
-- entries that sum up to @2020@.
--
-- >>> solve [1721, 979, 366, 299, 675, 1456] 2
-- 514579
--
-- | >>> solve [1721, 979, 366, 299, 675, 1456] 3
-- 241861950
solve ::
  [Int] {- ^ inputs  -} ->
  Int   {- ^ count   -} ->
  Int   {- ^ product -}
solve input n = solve' (sort input) n 2020 1 (error "no solution")

solve' ::
  [Int] {- ^ sorted inputs    -} ->
  Int   {- ^ count            -} ->
  Int   {- ^ target sum       -} ->
  Int   {- ^ current product  -} ->
  Int   {- ^ next alternative -} ->
  Int   {- ^ final product    -}
solve' _      0 0 p _ = p
solve' (x:xs) n s p e | 0 < n, x <= s = solve' xs (n-1) (s-x) (p*x) (solve' xs n s p e)
solve' _      _ _ _ e = e
