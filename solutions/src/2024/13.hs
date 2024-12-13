{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/13>

This solution doesn't handle the cases where
buttons A and B are colinear, but the input
didn't require us to worry about that.

>>> :{
:main + "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400
\&
Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176
\&
Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450
\&
Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"
:}
480
875318608908

-}
module Main (main) where

import Advent (format)

-- | >>> :main
-- 29877
-- 99423413811305
main :: IO ()
main =
 do input <- [format|2024 13
      (Button A: X%+%u, Y%+%u%n
      Button B: X%+%u, Y%+%u%n
      Prize: X=%u, Y=%u%n)&%n|]
    print (sum (map (cost              0) input))
    print (sum (map (cost 10000000000000) input))

cost :: Int -> (Int, Int, Int, Int, Int, Int) -> Integer
cost extra (ax, ay, bx, by, x, y)
  | det == 0 = error "colinearity not supported"
  | a >= 0, b >= 0, ar == 0, br == 0 = 3 * a + b
  | otherwise = 0
  where
    ax' = toInteger ax
    ay' = toInteger ay
    bx' = toInteger bx
    by' = toInteger by
    x'  = toInteger (x + extra)
    y'  = toInteger (y + extra)
    
    det = ax' * by' - ay' * bx'
    (a, ar) = (by' * x' - bx' * y') `quotRem` det
    (b, br) = (ax' * y' - ay' * x') `quotRem` det
