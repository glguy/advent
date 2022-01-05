{-# Language BangPatterns, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/24>

Build long bridges out of pieces with a pin count on each
end. Pieces can be flipped over and to be connected the pin
counts of two pieces must match.

-}
module Main where

import Advent (format)
import Data.List (delete)

-- | Print solutions to both parts of the task.
main :: IO ()
main =
  do input <- [format|2017 24 (%d/%d%n)*|]

     let bridges = search 0 0 0 input

     print (maximum (map snd bridges)) -- part 1: weights
     print (snd (maximum bridges))     -- part 2: lengths *then* weights

-- | Given a required number of ports and a piece, return the possible
-- unique orientations of that piece.
orient ::
  Int         {- ^ target left pin count               -} ->
  (Int,Int)   {- ^ current piece                       -} ->
  [(Int,Int)] {- ^ possible orientations of this piece -}
orient a (b,c)
  | a == b    = [(b,c)]
  | a == c    = [(c,b)]
  | otherwise = []

-- | Generate statistics for all of the possible bridges given some pieces.
search ::
  Int         {- ^ current bridge length                 -} ->
  Int         {- ^ current bridge weight                 -} ->
  Int         {- ^ required port pins                    -} ->
  [(Int,Int)] {- ^ available pieces                      -} ->
  [(Int,Int)] {- ^ length and weight of possible bridges -}
search !len !weight !match pieces =
  (len,weight) : -- values if we stopped here
  do piece <- pieces
     (a,b) <- orient match piece
     search (len+1) (weight+a+b) b (delete piece pieces)
