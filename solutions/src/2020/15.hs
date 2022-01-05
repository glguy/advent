{-# Language BlockArguments, NumericUnderscores, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/15>

-}
module Main (main) where

import Advent.Format (format)
import Control.Monad (zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Int (Int32)
import Data.Primitive.PrimArray (MutablePrimArray, readPrimArray, writePrimArray, newPinnedPrimArray, setPrimArray)

-- | Type of elements in our sequence -- big enough to hold 30 million
type T = Int32

-- |
-- >>> game [10,16,6,0,1,17] 2020
-- 412
main :: IO ()
main =
  do inp <- map fromIntegral <$> [format|2020 15 %u&,%n|]
     print (game inp      2_020)
     print (game inp 30_000_000)

game ::
  [T] {- ^ initial sequence -} ->
  T   {- ^ desired position -} ->
  T   {- ^ desired element  -}
game xs n = runST
  do let len = fromIntegral (max n (1 + last xs))
     a <- newPinnedPrimArray len
     setPrimArray a 0 len 0
     zipWithM_ (writePrimArray a) (fromIntegral <$> Prelude.init xs) [1..]
     speak a n (fromIntegral (length xs)) (last xs)

speak ::
  MutablePrimArray s T {- ^ position of last occurrence -} ->
  T      {- ^ desired position -} ->
  T      {- ^ current position -} ->
  T      {- ^ current element  -} ->
  ST s T {- ^ desired element  -}
speak a n m x
  | m == n    = pure $! x
  | otherwise = do v <- exchange a (fromIntegral x) m
                   speak a n (m+1) (if v == 0 then 0 else m-v)

-- | Exchange element at an index with a new element returning old element.
exchange :: MutablePrimArray s T -> Int -> T -> ST s T
exchange a i x = readPrimArray a i <* writePrimArray a i x
