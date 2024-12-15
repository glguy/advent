{-# Language QuasiQuotes, ImportQualifiedPost, LambdaCase, BlockArguments #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/14>

To find the answer to part 2 I tried a bunch of stuff.
What actually worked was finding pictures with many adjacent
pixels. I then updated my solution to find pictures without
overlapping pixels, which seemed to uniquely identify the answer.

-}
module Main (main) where

import Advent (counts, format)
import Advent.Coord (Coord(C), drawCoords, mapCoord, zipCoord)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Set qualified as Set

room :: Coord
room = C 103 101

main :: IO ()
main =
 do input <- [format|2024 14 (p=%d,%d v=%d,%d%n)*|]
    print (product (counts (concatMap (toQuad . runBot 100) input)))

    -- search for non-overlapping pictures. the picture has to loop after 103*101 steps.
    for_ [0 .. 103 * 101 - 1] \i ->
     do let pic = map (runBot i) input
        when (hasNoOverlap pic)
         do putStr (drawCoords pic)
            print i

-- | Predicate for lists of coordinates that do not overlap
hasNoOverlap :: [Coord] -> Bool
hasNoOverlap = go Set.empty
  where
    go seen = \case
      []     -> True
      x : xs -> Set.notMember x seen && go (Set.insert x seen) xs

-- | Figure out what quadrant a coordinate is in, if any.
toQuad :: Coord -> [(Ordering, Ordering)]
toQuad (C y x) =
  [ (xo, yo)
  | let C midY midX = mapCoord (`div` 2) room
  , let xo = compare x midX, xo /= EQ
  , let yo = compare y midY, yo /= EQ
  ]

-- | Run a bot for a certain number of time steps and find its endpoint.
runBot ::
  Int                  {- ^ time steps            -} ->
  (Int, Int, Int, Int) {- ^ location and velocity -} ->
  Coord                {- ^ destination           -}
runBot n (x, y, dx, dy) = zipCoord mod (C (y + n * dy) (x + n * dx)) room
