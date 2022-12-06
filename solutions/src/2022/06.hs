{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/6>

-}
module Main where

import Data.List (findIndex, tails)

import Advent (format, ordNub)

main :: IO ()
main =
 do input <- [format|2022 6 %s%n|]
    print (solve  4 input)
    print (solve 14 input)

solve :: Ord a => Int -> [a] -> Maybe Int
solve n input = fmap (n+) (findIndex (start n) (tails input))

start :: Ord a => Int -> [a] -> Bool
start n xs = length (ordNub (take n xs)) == n