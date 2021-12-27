{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 10 poses a convoluted knot-tying algorithm to implement.

-}
module Main where

import Advent           (getInputLines)
import Control.Monad    ((<=<), zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Bits        (xor)
import Data.Char        (ord)
import Data.Foldable    (for_)
import Data.List        (foldl1')
import Data.List.Split  (chunksOf, splitOn)
import Text.Printf      (printf)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as M

-- | Print the solution to both parts of Day 10. Input file is configurable
-- via the command-line.
main :: IO ()
main =
  do [inputLine] <- getInputLines 10
     putStrLn (part1 256 inputLine)
     putStrLn (part2 256 inputLine)

-- | Compute the product of the first two elements after performing
-- the knot-tying ritual using the lengths given as inputs.
--
-- >>> part1 5 "3,4,1,5"
-- "12"
part1 ::
  Int    {- ^ rope length  -} ->
  String {- ^ input string -} ->
  String {- ^ output hash  -}
part1 sz = show . product . take 2 . tieKnots sz . part1Input

-- | Given a rope size and an input string, compute the resulting hash.
--
-- >>> part2 256 ""
-- "a2582a3a0e66e6e86e3812dcb672a272"
-- >>> part2 256 "AoC 2017"
-- "33efeb34ea91902bb2f59c9920caa6cd"
-- >>> part2 256 "1,2,3"
-- "3efbe78a8d82f29979031a4aa0b16a9d"
-- >>> part2 256 "1,2,4"
-- "63960835bcdc130f0b66d7ff4f6a5a8e"
part2 ::
  Int    {- ^ rope length  -} ->
  String {- ^ input string -} ->
  String {- ^ output hash  -}
part2 sz = hash . tieKnots sz . concat . replicate 64 . part2Input

-- | Compute the "dense hash" of a of a rope. Rope length should
-- be a multiple of 16.
--
-- >>> hash [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22]
-- "40"
hash :: [Int] {- ^ rope -} -> String
hash = printf "%02x" . foldl1' xor <=< chunksOf 16

-- | Transform the input string according to the part 1 rule to
-- produce the list of knot lengths required.
--
-- >>> part1Input "1,2,3"
-- [1,2,3]
part1Input ::
  String {- ^ input string -} ->
  [Int]  {- ^ rope lengths -}
part1Input = map read . splitOn ","

-- | Transform the input string according to the part 2 rule to
-- produce the list of knot lengths required.
--
-- >>> part2Input "1,2,3"
-- [49,44,50,44,51,17,31,73,47,23]
part2Input ::
  String {- ^ input string -} ->
  [Int]  {- ^ rope lengths -}
part2Input str = map ord str ++ [17, 31, 73, 47, 23]

-- | Create a rope, tie knots of the given lengths while skipping
-- according to the increasing skip rule.
--
-- >>> tieKnots 5 [3, 4, 1, 5]
-- [3,4,2,1,0]
tieKnots ::
  Int   {- ^ rope size      -} ->
  [Int] {- ^ knot lengths   -} ->
  [Int] {- ^ resulting rope -}
tieKnots size lengths = runST $
  do v <- V.thaw (V.generate size id)
     let cursors = scanl (+) 0 (zipWith (+) [0 ..] lengths)
     zipWithM_ (tieKnot v) lengths cursors
     V.toList <$> V.unsafeFreeze v

-- | Reverse the length of elements starting at the given cursor.
tieKnot ::
  M.MVector s Int {- ^ rope vector     -} ->
  Int             {- ^ knot length     -} ->
  Int             {- ^ cursor position -} ->
  ST s ()
tieKnot v len cur =
  do let wrap x = x `mod` M.length v
     for_ [0 .. len`div`2 - 1] $ \i ->
        M.swap v (wrap (cur+i)) (wrap (cur + len - i - 1))
