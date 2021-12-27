{-# Language BlockArguments, ImportQualifiedPost, NumericUnderscores, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/23>

-}
module Main (main) where

import Advent.Format (format)
import Control.Monad (unless, zipWithM_)
import Data.Array.IO (IOUArray, getBounds, newArray_, readArray, writeArray)
import Data.Char (digitToInt)
import Data.Foldable (for_)

-- | The array maps cup numbers (indexes) to the next cup
-- in the sequence (elements).
type Ring = IOUArray Int Int

-- | Construct a /ring of cups/ given an initial arrangement and filled to
-- the given size.
newRing ::
  Int   {- ^ ring size           -} ->
  [Int] {- ^ initial arrangement -} ->
  IO Ring
newRing n order =
  do a <- newArray_ (1,n)
     for_ [1..n-1] \i -> writeArray a i (i+1)
     zipWithM_ (writeArray a) order (tail order)
     if n == length order
        then writeArray a (last order) (head order)
        else writeArray a n (head order) >> writeArray a (last order) (1+maximum order)
     pure a

readRing ::
  Ring ->
  Int {- ^ length       -} ->
  Int {- ^ starting cup -} ->
  IO [Int]
readRing a n i
  | n <= 0 = pure []
  | otherwise =
    do j    <- readArray a i
       rest <- readRing a (n-1) j
       pure (j : rest)

-- |
-- >>> :main
-- 47382659
-- 42271866720
main :: IO ()
main =
  do inp <- map digitToInt <$> [format|23 %s%n|]
     p1 inp
     p2 inp

play ::
  Ring ->
  Int {- ^ iterations -} ->
  Int {- ^ current cup -} ->
  IO ()
play a i cur =
  unless (i==0)
  do -- extract a group of three cups
     g1 <- readArray a cur
     g2 <- readArray a g1
     g3 <- readArray a g2

     -- find next cup and link current one to it
     nx <- readArray a g3
     writeArray a cur nx

     -- find the new destination label
     (_,hi) <- getBounds a
     let dec 1 = hi
         dec x = x - 1
         dest = until (\x -> x /= g1 && x /= g2 && x /= g3) dec (dec cur)

     -- splice the group back in at dest
     writeArray a g3 =<< readArray a dest
     writeArray a dest g1

     play a (i-1) nx

p1 :: [Int] -> IO ()
p1 inp =
  do ring <- newRing (length inp) inp
     let sz = length inp
     play ring 100 (head inp)

     xs <- readRing ring (sz-1) 1
     putStrLn (concatMap show xs)

p2 :: [Int] -> IO ()
p2 inp =
  do let sz   =  1_000_000
         iter = 10_000_000

     ring <- newRing sz inp
     play ring iter (head inp)

     xs <- readRing ring 2 1
     print (product xs)
