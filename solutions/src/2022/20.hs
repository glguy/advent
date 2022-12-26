{-# Language QuasiQuotes, ImportQualifiedPost, NumericUnderscores, BlockArguments, BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/20>

>>> :main + "1\n2\n-3\n3\n-2\n0\n4\n"
3
1623178306

-}
module Main where

import Control.Monad (replicateM_)
import Data.Array.IO (IOUArray, newListArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, (!), listArray)
import Data.Foldable (for_)
import Data.List (elemIndex)

import Advent (format, timesM)

-- |
-- >>> :main
-- 1591
-- 14579387544492
main :: IO ()
main = do
    input <- [format|2022 20 (%d%n)*|]
    print =<< solve 1 input
    print =<< solve 10 (map (811_589_153*) input)

solve :: Int -> [Int] -> IO Int
solve iterations xs =
 do let n = length xs
    let inputArray = listArray (0, n-1) xs :: UArray Int Int
    ring <- newRing n

    replicateM_ iterations $
      for_ [0..n-1] \i ->
       do let d = inputArray!i `mod` (n-1)
              d' = if d > n`div`2 then d-(n-1) else d
          a <- removeRing i ring
          a' <- walk d' a ring
          insertBeforeRing i a' ring

    let Just z = elemIndex 0 xs
    i1 <- walk 1_000 z  ring
    i2 <- walk 1_000 i1 ring
    i3 <- walk 1_000 i2 ring
    pure $! sum [inputArray!i1, inputArray!i2, inputArray!i3]

data Ring = Ring {
    fwdLinks :: !(IOUArray Int Int), -- ^ forward links
    bwdLinks :: !(IOUArray Int Int)  -- ^ backward links
}

-- | Build a new circular ring of given size
newRing :: Int {- ^ size -} -> IO Ring
newRing n =
    Ring <$> newListArray (0,n-1) ([1..n-1]++[0])
         <*> newListArray (0,n-1) ((n-1):[0..n-2])

-- | Remove a node from the ring and return the index of the node before/after it.
removeRing ::
  Int {- ^ node to remove from ring -} ->
  Ring {- ^ ring -} ->
  IO Int {- ^ index after removed node -}
removeRing i ring =
 do prev <- readArray (bwdLinks ring) i
    next <- readArray (fwdLinks ring) i
    writeArray (fwdLinks ring) prev next
    writeArray (bwdLinks ring) next prev
    pure next

insertBeforeRing ::
  Int {- ^ node to insert -} ->
  Int {- ^ node to insert before -} ->
  Ring {- ^ ring -} ->
  IO ()
insertBeforeRing node next ring =
 do prev <- readArray (bwdLinks ring) next
    writeArray (fwdLinks ring) node next
    writeArray (bwdLinks ring) node prev
    writeArray (fwdLinks ring) prev node
    writeArray (bwdLinks ring) next node

walk ::
  Int {- ^ step count (positive for forward, negative for backward) -} ->
  Int {- ^ starting index -} ->
  Ring {- ^ ring -} ->
  IO Int {- ^ ending index -}
walk n i !ring
  | n < 0     = timesM (-n) (readArray (bwdLinks ring)) i
  | otherwise = timesM   n  (readArray (fwdLinks ring)) i
