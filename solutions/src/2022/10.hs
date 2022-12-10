{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/10>

-}
module Main where

import Advent (chunks, format)

-- | Process input file to print day 10 solution
--
-- >>> :main
-- 14360
-- ███░░░██░░█░░█░░██░░████░███░░████░████░
-- █░░█░█░░█░█░█░░█░░█░█░░░░█░░█░█░░░░░░░█░
-- ███░░█░░░░██░░░█░░█░███░░█░░█░███░░░░█░░
-- █░░█░█░██░█░█░░████░█░░░░███░░█░░░░░█░░░
-- █░░█░█░░█░█░█░░█░░█░█░░░░█░█░░█░░░░█░░░░
-- ███░░░███░█░░█░█░░█░████░█░░█░████░████░
main :: IO ()
main =
 do input <- [format|2022 10 ((noop|addx %d)%n)*|]
    let xs = execute input
    print (sum [i * xs!!(i-1) | i <- [20,60,100,140,180,220]])
    putStr (crt xs)

-- | Generate the stream of X values on each cycle when executing a program.
--
-- >>> execute [Nothing, Just 3, Just (-5)]
-- [1,1,1,4,4,-1]
execute :: [Maybe Int] -> [Int]
execute = go 1
  where
    go x []             = [x]
    go x (Nothing : xs) = x :     go x xs
    go x (Just d  : xs) = x : x : go (d+x) xs

-- | Predicate for @-1@, @0@, and @1@.
isNear :: Int -> Int -> Bool
isNear x y = abs (x - y) <= 1

-- | Treat the input as a horizontal sprite position and render it into
-- 40 character lines.
crt :: [Int] -> String
crt xs = unlines [ [if near then '█' else '░' | near <- zipWith isNear [0..] row]
                 | row <- take 6 (chunks 40 xs)]