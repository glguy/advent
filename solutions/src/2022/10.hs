{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/10>

>>> :{
:main + "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\n\
        \addx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\n\
        \addx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\n\
        \noop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\n\
        \noop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\n\
        \noop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\n\
        \addx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\n\
        \addx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\n\
        \noop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\n\
        \addx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\n\
        \addx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\n\
        \noop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\n\
        \noop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop\n"
:}
13140
██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░
███░░░███░░░███░░░███░░░███░░░███░░░███░
████░░░░████░░░░████░░░░████░░░░████░░░░
█████░░░░░█████░░░░░█████░░░░░█████░░░░░
██████░░░░░░██████░░░░░░██████░░░░░░████
███████░░░░░░░███████░░░░░░░███████░░░░░

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
