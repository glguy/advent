{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/14>

>>> :set -XQuasiQuotes
>>> let input = [format|- ((%u,%u)&( -> )%n)*|] "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n"
>>> let world = Set.fromList (concatMap segs input)
>>> let limit = voidLimit world
>>> let Left world1 = fillFrom Left limit world top
>>> let picture = Data.Map.fromSet (const '█') world <> Data.Map.fromSet (const '◆') world1
>>> putStr (Advent.Coord.drawPicture picture)
······◆···
·····◆◆◆··
····█◆◆◆██
···◆█◆◆◆█·
··███◆◆◆█·
····◆◆◆◆█·
·◆·◆◆◆◆◆█·
█████████·
>>> Set.size world1 - Set.size world
24
>>> let Identity world2 = fillFrom Identity limit world top
>>> let picture = Data.Map.fromSet (const '█') world <> Data.Map.fromSet (const '◆') world2
>>> putStr (Advent.Coord.drawPicture picture)
··········◆··········
·········◆◆◆·········
········◆◆◆◆◆········
·······◆◆◆◆◆◆◆·······
······◆◆█◆◆◆██◆······
·····◆◆◆█◆◆◆█◆◆◆·····
····◆◆███◆◆◆█◆◆◆◆····
···◆◆◆◆·◆◆◆◆█◆◆◆◆◆···
··◆◆◆◆◆◆◆◆◆◆█◆◆◆◆◆◆··
·◆◆◆█████████◆◆◆◆◆◆◆·
◆◆◆◆◆·······◆◆◆◆◆◆◆◆◆
>>> Set.size world2 - Set.size world
93

-}
module Main where

import Control.Monad (foldM)
import Data.Functor.Identity (Identity(Identity))
import Data.List (find, foldl')
import Data.Set (Set)
import Data.Set qualified as Set

import Advent (format)
import Advent.Coord (below, coordRow, left, right, Coord(..))

-- |
-- >>> :main
-- 644
-- 27324
main :: IO ()
main =
 do input <- [format|2022 14 ((%u,%u)&( -> )%n)*|]
    let world = Set.fromList (concatMap segs input)
        limit = voidLimit world

    case fillFrom Left limit world top of
      Right _     -> fail "no solution"
      Left world1 -> print (Set.size world1 - Set.size world)

    case fillFrom Identity limit world top of
      Identity world2 -> print (Set.size world2 - Set.size world)

-- | The entry point of sand at @500,0@
top :: Coord
top = C 0 500

voidLimit :: Set Coord -> Int
voidLimit world = 2 + maximum [y | C y _ <- Set.toList world]

-- | Fill the given world with sand from a fill coordinate returning the
-- final state of the world. This is parameterized over a callback for how
-- to handle when sand reaches the bottom of the level in order to allow
-- early termination or not.
fillFrom ::
  Monad m =>
  (Set Coord -> m (Set Coord)) {- ^ behavior when sand reaches limit -} ->
  Int                          {- ^ lower limit -} ->
  Set Coord                    {- ^ initial wall and sand locations -} ->
  Coord                        {- ^ location to fill from -} ->
  m (Set Coord)                {- ^ final wall and sand locations -}
fillFrom onVoid limit world here
  | limit == coordRow here = onVoid world
  | Set.member here world = pure world
  | otherwise = Set.insert here <$> foldM (fillFrom onVoid limit) world
                  [below here, left (below here), right (below here)]

-- Turning line segments into sets of coordinates

segs :: [(Int,Int)] -> [Coord]
segs (x:y:z) = seg x y ++ segs (y:z)
segs [(x,y)] = [C y x]
segs []      = []

seg :: (Int,Int) -> (Int,Int) -> [Coord]
seg (a,b) (c,d)
  | a == c    = [C y a | y <- [min b d .. max b d]]
  | b == d    = [C d x | x <- [min a c .. max a c]]
  | otherwise = error "unexpected input"
