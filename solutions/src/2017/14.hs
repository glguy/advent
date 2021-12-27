{-# Language QuasiQuotes, DataKinds #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 14 ties Day 10 and Day 12 together presumably to see how quickly
we can combine our previous results to make something new.

-}
module Main where

import           Advent
import           Advent.Permutation
import           Advent.Coord
import           Control.Monad
import           Control.Monad.ST (ST, runST)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Graph.Inductive
import           Data.List
import           Data.List.Split
import           Data.Word

-- | Compute the solution to Day 14. Input can be overriden via the
-- command-line.
main :: IO ()
main =
  do input <- [format|14 %s%n|]

     let g = coordsToGraph (gridToCoords (buildGrid input))

     print (noNodes g)
     print (noComponents g)


-- | Convert the set of coordinates into a graph labeled with those
-- coordinates where adjacent elements have edges between them.
coordsToGraph :: Set Coord -> Gr Coord ()
coordsToGraph coords = run_ empty $
  do insMapNodesM (Set.toList coords)
     insMapEdgesM [ (src,dst,())
                    | src <- Set.toList coords
                    , dst <- cardinal src
                    , Set.member dst coords ]

-- | Build the problem grid as a list of rows where a cell is set in
-- a row is set when the bit at that index is set.
buildGrid :: String -> V.Vector Integer
buildGrid str = V.generate 128 $ \i -> knotHash $ str ++ "-" ++ show i

-- | Convert a grid into a list of coordinates that are set.
gridToCoords :: V.Vector Integer -> Set Coord
gridToCoords grid = Set.fromList
  [ C r c | (r,row) <- zip [0..] (V.toList grid)
          , c       <- [0..127]
          , testBit row c]


-- Stuff ripped out of day 10 --

-- | Given a rope size and an input string, compute the resulting hash.
knotHash ::
  String  {- ^ input string -} ->
  Integer {- ^ knot value   -}
knotHash =
   bytesToInteger . map (foldl1' xor) .
   chunksOf 16 . tieKnots . concat . replicate 64 .
   (++ [17, 31, 73, 47, 23]) .  map ord

-- | Convert list of bytes into integer in big-endian order.
bytesToInteger :: [Word8] -> Integer
bytesToInteger = foldl' (\acc x -> acc * 0x100 + fromIntegral x) 0

-- | Create a rope, tie knots of the given lengths while skipping
-- according to the increasing skip rule.
tieKnots ::
  [Int]   {- ^ knot lengths   -} ->
  [Word8] {- ^ resulting rope -}
tieKnots lengths = runPermutation fromIntegral
                 $ mconcat [ p o l
                           | (o,l) <- zip (scanl (+) 0 (zipWith (+) [0..] lengths)) lengths
                           ]

p :: Int -> Int -> Permutation 256
p o l = mkPermutation $ \i -> if (i-o)`mod`256 < l
                                   then l-1-i+o+o
                                   else i

-- | Reverse the length of elements starting at the given cursor.
tieKnot ::
  M.MVector s Word8 {- ^ rope vector     -} ->
  Int               {- ^ knot length     -} ->
  Int               {- ^ cursor position -} ->
  ST s ()
tieKnot v len cur =
  do let wrap x = x `mod` M.length v
     for_ [0 .. len`div`2 - 1] $ \i ->
        M.swap v (wrap (cur+i)) (wrap (cur + len - i - 1))
