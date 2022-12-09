{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/9>

-}
module Main where

import Advent ( format, ordNub, stageTH )
import Advent.Coord ( above, below, left, origin, right, Coord(C) )
import Data.Set qualified as Set

data C = CD | CR | CU | CL deriving Show

stageTH

-- |
-- >>> :main
-- 5930
-- 2443
main :: IO ()
main = do
    input <- [format|2022 9 (@C %u%n)*|]
    let knots = infiniteRope (concatMap expand input)
    print (length (Set.fromList (knots !! 1)))
    print (length (Set.fromList (knots !! 9)))

-- | Generate a stream of lists, one for each knot in the rope.
infiniteRope :: [C] -> [[Coord]]
infiniteRope = iterate (scanl1 updateTail) . scanl drive origin

expand :: (a, Int) -> [a]
expand (x,n) = replicate n x

drive :: Coord -> C -> Coord
drive here CD = below here
drive here CU = above here
drive here CL = left  here
drive here CR = right here

updateTail ::
  Coord {- ^ tail -} ->
  Coord {- ^ head -} ->
  Coord
updateTail t@(C ty tx) h@(C hy hx)
  | touching ty hy, touching tx hx = t
  | otherwise = t + signum (h-t)

touching :: Int -> Int -> Bool
touching x y = abs (x - y) < 2
