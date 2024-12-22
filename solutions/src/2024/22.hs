{-# Language QuasiQuotes, ImportQualifiedPost, BangPatterns #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/22>

>>> :{
:main + "1
2
3
4
2024
"
:}
37990510
23

-}
module Main (main) where

import Advent (format, times)
import Data.Array.Unboxed (UArray, accumArray, assocs, elems)
import Data.Bits (xor)

-- | >>> :main
-- 19241711734
-- 2058
main :: IO ()
main =
 do input <- [format|2024 22 (%u%n)*|]
    print (sum (map (times 2000 next) input))
    let a = accumArray (+) 0 ((-9,-9,-9,-9),(9,9,9,9))
              [(k,v) | i <- input, (k,v) <- assocs (char i), v > 0]
              :: UArray (Int,Int,Int,Int) Int
    print (maximum (elems a))

-- | Generate the next secret number
--
-- >>> map next [1, 10, 100, 2024]
-- [8685429, 4700978, 15273692, 8667524]
next :: Int -> Int
next x = c
  where
    prune y = y `mod` 16777216
    a = prune (x `xor` (x * 64))
    b = prune (a `xor` (a `div` 32))
    c = prune (b `xor` (b * 2048))

char :: Int -> UArray (Int,Int,Int,Int) Int
char x = accumArray upd (-1) ((-9,-9,-9,-9),(9,9,9,9))
          (gen 4 (delta x x1) (delta x1 x2) (delta x2 x3) (delta x3 x4) x4)
  where
    upd x y = if x == -1 then y else x

    x1 = next  x
    x2 = next x1
    x3 = next x2
    x4 = next x3

    delta x y = y`mod`10 - x`mod`10

    gen !i !a !b !c !d !x
      | i <= 2000 = ((a,b,c,d), x`mod`10) : gen (i+1) b c d (delta x x') x'
      | otherwise = []
      where x' = next x
