{-# Language QuasiQuotes, NumDecimals #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/13>

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
import GHC.Num (integerGcde)

-- | >>> :main
-- 29877
-- 99423413811305
main :: IO ()
main =
 do input <- [format|2024 13
      (Button A: X%+%lu, Y%+%lu%n
      Button B: X%+%lu, Y%+%lu%n
      Prize: X=%lu, Y=%lu%n)&%n|]
    print (sum (map (cost    0) input))
    print (sum (map (cost 1e13) input))

cost :: Integer -> (Integer, Integer, Integer, Integer, Integer, Integer) -> Integer
cost extra (ax, ay, bx, by, x, y)
  | det == 0 = colinear ax ay bx by x y
  | (a, 0) <- (by * x' - bx * y') `quotRem` det, a >= 0
  , (b, 0) <- (ax * y' - ay * x') `quotRem` det, b >= 0
  = 3 * a + b
  | otherwise = 0
  where
    det = ax * by - ay * bx
    x'  = x + extra
    y'  = y + extra

-- | The buttons are colinear so we'll need to select a different
-- approach.
colinear ax ay bx by x y
  -- Buttons weren't colinear with the output - 0 is failure
  | ax * y /= ay * x || bx * y /= by * x = 0

  | ax /= 0, bx /= 0 = colinear' ax bx x
  | ay /= 0, by /= 0 = colinear' ay by y

  | ax /= 0 = 3 * single ax x
  | ay /= 0 = 3 * single ay y
  | bx /= 0 =     single bx x
  | by /= 0 =     single by y

  | otherwise = 0

-- | Only one button matters, just count the presses needed
single :: Integer -> Integer -> Integer
single ax x
  | (p, 0) <- x `quotRem` ax = p
  | otherwise = 0

-- <https://en.wikipedia.org/wiki/Diophantine_equation>
colinear' :: Integer -> Integer -> Integer -> Integer
colinear' ax bx x
  | let (d, e, f) = integerGcde ax bx
  , (h, 0) <- x `quotRem` d -- target x is a multiple of the gcd of the two button +x
  , let a = e * h -- prototypical (but potentially negative) solution
  , let b = f * h -- prototypical (but potentially negative) solution
  , let u = ax `quot` d
  , let v = - (bx `quot` d)
  , let klo = a `divCeil` v
  , let khi = b `div` u
  , klo <= khi
  = minimum [3 * (a - k * v) + (b - k * u) | k <- [klo, khi]] -- the relation is linear so one of the extremes is the answer
  | otherwise = 0

-- Division that rounds up
divCeil :: Integral a => a -> a -> a
x `divCeil` y = (x + y - 1) `div` y
