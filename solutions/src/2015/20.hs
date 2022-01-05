{-# Language QuasiQuotes, BlockArguments #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/20>

Run a simulation of elves delivering presents which each elf taking a larger
step size than the previous.

-}
module Main where

import Advent.Format (format)
import Control.Monad.Loop (for, exec_)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Data.Array.ST (readArray, writeArray, MArray(newArray), runSTUArray)
import Data.Array.Unboxed (UArray, assocs)

-- | >>> :main
-- 831600
-- 884520
main :: IO ()
main =
 do target <- [format|2015 20 %u%n|]
    print (findHouse target (solve1 target))
    print (findHouse target (solve2 target))

-- | Return the house number with at least the given number of presents.
findHouse :: Int -> UArray Int Int -> Int
findHouse target a = head [h | (h,t) <- assocs a, t >= target]

solve1 :: Int -> UArray Int Int
solve1 target = runSTUArray
 do let top = target `quot` 10
    a <- newArray (1,top) 0
    a <$ exec_
     do elf   <- for   1 (<= top) (1+)
        house <- for elf (<= top) (elf+)
        lift do old <- readArray a house
                writeArray a house (old + elf*10)

solve2 :: Int -> UArray Int Int
solve2 target = runSTUArray
 do let top = target `quot` 11
    a <- newArray (1,top) 0
    a <$ exec_
     do elf   <- for   1 (<=top) (1+)
        house <- for elf (<= min top (elf*50)) (+elf)
        lift do old <- readArray a house
                writeArray a house (old + elf*11)
