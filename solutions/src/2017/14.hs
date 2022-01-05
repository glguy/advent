{-# Language QuasiQuotes, DataKinds, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/14>

Day 14 ties Day 10 and Day 12 together presumably to see how quickly
we can combine our previous results to make something new.

-}
module Main (main) where

import Advent (format)
import Advent.Coord (cardinal, Coord(..))
import KnotHash (knotHash)
import Data.Vector qualified as V
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Bits (Bits(testBit))
import Data.Graph.Inductive

-- | Compute the solution to Day 14. Input can be overriden via the
-- command-line.
--
-- 8106
-- 1164
main :: IO ()
main =
  do input <- [format|2017 14 %s%n|]

     let g = coordsToGraph (gridToCoords (buildGrid input))

     print (noNodes g)
     print (noComponents g)


-- | Convert the set of coordinates into a graph labeled with those
-- coordinates where adjacent elements have edges between them.
coordsToGraph :: Set Coord -> Gr Coord ()
coordsToGraph coords = run_ empty $
  do _ <- insMapNodesM (Set.toList coords)
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
