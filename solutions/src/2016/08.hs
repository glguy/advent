{-# Language QuasiQuotes, TupleSections #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/8>

Run a series of pixel rotation commands to find the
solution image.

-}
module Main (main) where

import Advent (format, countBy)
import Advent.Coord (Coord(..), drawCoords)
import Control.Monad (when)
import Data.Array.IO
import Data.Foldable (for_, traverse_)

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

-- | >>> :main
-- 128
-- ████  ██   ██  ███   ██  ███  █  █ █   █ ██   ██
-- █    █  █ █  █ █  █ █  █ █  █ █  █ █   ██  █ █  █
-- ███  █  █ █  █ █  █ █    █  █ ████  █ █ █  █ █  █
-- █    █  █ ████ ███  █ ██ ███  █  █   █  ████ █  █
-- █    █  █ █  █ █ █  █  █ █    █  █   █  █  █ █  █
-- ████  ██  █  █ █  █  ███ █    █  █   █  █  █  ██
main :: IO ()
main =
 do input <- [format|2016 2016 8 ((rect %ux%u|rotate row y=%u by %u|rotate column x=%u by %u)%n)*|]
    let cmds = map toCommand input
    a <- newArray (C 0 0,C (rows-1) (cols-1)) False
          :: IO (IOUArray Coord Bool)
    traverse_ (interpCommand a) cmds
    print =<< countPixels a
    drawScreen a

drawScreen :: IOUArray Coord Bool -> IO ()
drawScreen a =
 do xs <- getAssocs a
    putStr (drawCoords [c | (c, True) <- xs])

countPixels :: IOUArray Coord Bool -> IO Int
countPixels a =
  do xs <- getElems a
     return $! countBy id xs

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
