{-# Language MagicHash, UnboxedSums #-}
module Main where

import GHC.Natural (Natural)
import GHC.Num.Integer (integerPowMod#)

main :: IO ()
main = print (code 3010 3019)

code ::
  Integer {- ^ row    -} ->
  Integer {- ^ column -} ->
  Integer
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
