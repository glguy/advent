{-# Language TemplateHaskell, ImportQualifiedPost, QuasiQuotes, ViewPatterns #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/22>

This solution processes commands in order and tracks the
currently lit regions with a list of disjoint cubes. When
processing a new cube, the cube is deleted from the list of
lit cubes and then added back in as a whole cube if the command
would turn lights on.

A slower, but also popular solution using the Inclusion-Exclusion Principle
can be found in 'solveInEx'.

<https://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle>

-}
module Main (main) where

import Advent.Box
import Advent (format, stageTH)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Foldable (foldl')
-- | On and off commands from the input file
data C = Con {- ^ lights on -} | Coff {- ^ lights off -}
  deriving (Show, Eq)

stageTH

-- | >>> :main
-- 606484
-- 1162571910364852
main :: IO ()
main =
 do inp <- [format|2021 22 (@C x=%d..%d,y=%d..%d,z=%d..%d%n)*|]
    let dim lo hi = Dim lo (hi+1) -- make upper bound exclusive
        steps = [ (c, dim x1 x2 (dim y1 y2 (dim z1 z2 Pt)))
                | (c, x1, x2, y1, y2, z1, z2) <- inp]
        p1dim = dim (-50) 50
        p1box = p1dim (p1dim (p1dim Pt))
    print (solve [(c, b) | (c, intersectBox p1box -> Just b) <- steps])
    print (solve steps)

-- | Figure out how many lights the given instructions turn on.
solve :: [(C, Box n)] -> Int
solve = sum . map size . foldl applyCommand []

-- | Apply a command given a list of non-overlapping, illuminated regions.
applyCommand ::
  [Box n]    {- ^ pre-lit boxes  -} ->
  (C, Box n) {- ^ command        -} ->
  [Box n]    {- ^ post-lit boxes -}
applyCommand ons (c, b) = [b | Con == c] ++ concatMap (subtractBox b) ons

-- * Alternative solution

-- | This solution uses the inclusion-exclusion principle instead
-- of relying on 'subtractBox'
solveInEx :: [(C, Box n)] -> Int
solveInEx cmds =
  sum [size k * v | (k,v) <- Map.assocs (foldl' addCommandInEx Map.empty cmds)]

-- | Update the inclusion and exclusion regions based on a single command.
-- This uses the same logic as 'solve' in that it turns off all the lights
-- in the given region first and then it turns them back on if the command
-- is /on/ and leaves them off if the command is /off/.
addCommandInEx ::
  Map (Box n) Int {- ^ inclusions and exclusions -} ->
  (C, Box n)      {- ^ command and region        -} ->
  Map (Box n) Int {- ^ inclusions and exclusions -}
addCommandInEx boxes (cmd, box)
  = Map.filter (0 /=) -- optimization to avoid finding intersections that don't matter
  $ Map.unionWith (+) boxes
  $ Map.fromListWith (+)
  $ [(box,1) | cmd == Con] ++
    [(k, -v) | (intersectBox box -> Just k,v) <- Map.assocs boxes] 
