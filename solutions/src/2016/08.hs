{-# Language QuasiQuotes, TupleSections #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/8>

-}
module Main (main) where

import Advent
import Advent.Coord
import Control.Concurrent
import Control.Monad
import Data.Array.IO
import Data.Foldable


rows, cols :: Int
rows = 6
cols = 50

data Command
  = Rect      !Int !Int
  | RotateCol !Int !Int
  | RotateRow !Int !Int
  deriving Show

toCommand :: Either (Either (Int, Int) (Int, Int)) (Int, Int) -> Command
toCommand (Left (Left (x,y))) = Rect x y
toCommand (Left (Right (y,n))) = RotateRow y n
toCommand (Right (x,n)) = RotateCol x n

main :: IO ()
main =
  do xs <- [format|8 ((rect %ux%u|rotate row y=%u by %u|rotate column x=%u by %u)%n)*|]
     
     interp (toCommand <$> xs)

interp :: [Command] -> IO ()
interp cmds =
  do a <- newArray (C 0 0,C (rows-1) (cols-1)) False
          :: IO (IOUArray Coord Bool)

     for_ cmds $ \cmd ->
       do interpCommand a cmd
          print cmd
          drawScreen a
          threadDelay 25000

     n <- countPixels a
     putStrLn ("Pixels: " ++ show n)

drawScreen :: IOUArray Coord Bool -> IO ()
drawScreen a =
  for_ [0..5] $ \y ->
    do xs <- traverse (\x -> readArray a (C y x)) [0..cols-1]
       putStrLn (map toBlock xs)

countPixels :: IOUArray Coord Bool -> IO Int
countPixels a =
  do xs <- getElems a
     return $! countBy id xs

toBlock :: Bool -> Char
toBlock True  = '█'
toBlock False = ' '

interpCommand :: IOUArray Coord Bool -> Command -> IO ()
interpCommand a (Rect xn yn) =
  for_ [0 .. xn-1] $ \x ->
  for_ [0 .. yn-1] $ \y ->
  writeArray a (C y x) True

interpCommand a (RotateCol x n) = rotate a (`C` x) rows n
interpCommand a (RotateRow y n) = rotate a (C y) cols n

rotate :: (Ix i, MArray a e m) => a i e -> (Int -> i) -> Int -> Int -> m ()
rotate a f len n =
  do reverseRange a f 0 (len-1-n)
     reverseRange a f (len-n) (len-1)
     reverseRange a f 0 (len-1)

reverseRange :: (Ix i, MArray a e m) => a i e -> (Int -> i) -> Int -> Int -> m ()
reverseRange a f lo hi =
  when (lo < hi) $
    do swap a (f lo) (f hi)
       reverseRange a f (lo+1) (hi-1)

swap :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swap a i j =
  do t <- readArray a i
     writeArray a i =<< readArray a j
     writeArray a j t
