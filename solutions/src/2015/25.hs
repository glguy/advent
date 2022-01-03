{-# Language MagicHash, UnboxedSums, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/25>

-}
module Main where

import Advent.Format (format)
import GHC.Natural (Natural)
import GHC.Num.Integer (integerPowMod#)

-- | >>> :main
-- 8997277
main :: IO ()
main =
 do (row,col) <- [format|25 To continue, please consult the code grid in the manual.  Enter the code at row %lu, column %lu.%n|]
    print (code row col)

-- | Compute the value at a location on Santa's infinite sheet of paper. 
code ::
  Integer {- ^ row    -} ->
  Integer {- ^ column -} ->
  Integer {- ^ cell value -}
code row col
  = 20151125
  * powModInteger 252533 (cell (row-1) (col-1)) 33554393
  `mod` 33554393

powModInteger :: Integer -> Integer -> Natural -> Integer 
powModInteger x y m =
  case integerPowMod# x y m of
    (# x | #) -> toInteger x
    (# | _ #) -> error "powModInteger: bad argument"

-- | Compute zero-indexed cell of diagonally filled table using zero-indexed row, column.
cell ::
  Integer {- ^ row    -} ->
  Integer {- ^ column -} ->
  Integer
cell r c = sum1N (r+c) + c

-- | Compute sum of non-negative integers from 0 to the given upper bound.
sum1N :: Integer {- ^ upper bound -} -> Integer
sum1N n = n*(n+1)`quot`2
