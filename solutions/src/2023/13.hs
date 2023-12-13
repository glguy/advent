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
    print (sum (concatMap (solver 0) input))
    print (sum (concatMap (solver 1) input))

findReflection :: Int {- ^ differences -} -> [String] -> [Int]
findReflection target xs =
  [ i
  | (i,l,r) <- zip3 [0..] (inits xs) (tails xs)
  , not (null l), not (null r)
  , target == sum (zipWith (\x y -> sum (zipWith val x y)) (reverse l) r)
  ]

solver :: Int -> [String] -> [Int]
solver target xs = findReflection target (transpose xs)
                ++ map (100*) (findReflection target xs)

val :: Char -> Char -> Int
val x y = if x == y then 0 else 1
