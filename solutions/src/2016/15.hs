{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/15>

-}
module Main where

import Advent (format)
import Advent.Chinese (chinese, toMod)

-- | >>> :main
-- 376777
-- 3903937
main :: IO ()
main =
 do input <- [format|2016 15 (Disc #%lu has %lu positions; at time=%lu, it is at position %lu.%n)*|]
    let input1 = [toMod (-p-i+t) n | (i, n, t, p) <- input] 
    let input2 = input1 ++ [toMod (-fromIntegral (length input1)-1) 11]
    print (chinese input1)
    print (chinese input2)
