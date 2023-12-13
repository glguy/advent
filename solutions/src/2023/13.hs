{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/13>

>>> :{
:main +
"#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.\n
#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
"
:}
405
400

-}
module Main (main) where

import Advent (format)
import Data.List (inits, tails, transpose)

-- |
--
-- >>> :main
-- 28895
-- 31603
main :: IO ()
main =
 do input <- [format|2023 13 (%s%n)*&%n|]
    print (sum (map (solver 0) input))
    print (sum (map (solver 1) input))

findReflection :: Int {- ^ differences -} -> [String] -> [Int]
findReflection target xs =
  [ i
  | (i, l, r) <- zip3 [0..] (inits xs) (tails xs)
  , not (null l), not (null r)
  , let diff x y = if x == y then 0 else 1
  , target == sum2 (sum2 diff) (reverse l) r
  ]

sum2 :: Num c => (a -> b -> c) -> [a] -> [b] -> c
sum2 f xs ys = sum (zipWith f xs ys)

solver :: Int -> [String] -> Int
solver n xs =
  head (findReflection n (transpose xs) ++ map (100 *) (findReflection n xs))
