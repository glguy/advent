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
import Data.List (tails, transpose)

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

findReflection :: Int -> [String] -> [Int]
findReflection differences xs =
  [ i
  | (i, l, r) <- zip3 [0..] (inits' xs) (tails xs)
  , not (null l), not (null r)
  , let diff x y = if x == y then 0 else 1
  , differences == sum2 (sum2 diff) l r
  ]

solver :: Int -> [String] -> Int
solver n xs =
  head (findReflection n (transpose xs) ++ map (100 *) (findReflection n xs))

-- | Like inits, but the prefixes are built up in reverse
-- >>> inits' [1,2,3]
-- [[],[1],[2,1],[3,2,1]]
inits' :: [a] -> [[a]]
inits' = scanl (flip (:)) []

-- | Kind of a generalized dot-product. Trims off longer list.
-- >>> sum2 (*) [2,3] [10,100,1000]
-- 320
sum2 :: Num c => (a -> b -> c) -> [a] -> [b] -> c
sum2 f xs ys = sum (zipWith f xs ys)
