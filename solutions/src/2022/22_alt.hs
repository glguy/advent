{-# Language QuasiQuotes, ConstraintKinds, TemplateHaskell, ImportQualifiedPost, LambdaCase, ImplicitParams, DataKinds #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/22>

This solution works by first exploring the input file and assigning a cube
location to each flattened location. The path is explored in terms of the cube
coordinates and then is converted back into input file coordinates at the end.

>>> :{
:main + "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.
\&
10R5L5R10L4R5L5
"
:}
5031

-}
module Main (main) where

import Advent (stageTH, format, countBy)
import Advent.Coord (Coord(..), coordLines, origin, above, below, left, right)
import Advent.Permutation (Permutation, mkPermutation, invert)
import Advent.Search (dfsOn)
import Control.Monad (msum)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set

-- | Left and right turns
data D = DL | DR

stageTH

-- | Constraint for upper bound of cube coordinates
type HiVal = ?hiVal :: Int

-- |
-- >>> :main
-- 55267
main :: IO ()
main =
 do (rawmap, path) <- [format|2022 22 (( |.|#)*!%n)*%n(%u|@D)*%n|]

    -- figure out the side-length of the cube we're working with
    -- so that we can handle both examples and regular inputs
    let elts = countBy (`elem` ".#") (concat rawmap)
    let ?hiVal = until (\x -> 6*x*x >= elts) (1 +) 1 - 1

    -- associate cube coordinates with all of the input file coordinates
    let maze = explore (Set.fromList [c | (c, '.') <- coordLines rawmap])

    -- figure out the cube coordinate that our path ends on
    let (C y x, facing) = locToFlat maze (foldl (flip (applyCommand maze)) locOrigin path)

    -- compute the "password" from the end location
    print (1000 * (y + 1) + 4 * (x + 1) + facing)

-- | Given the set of flat path coordinates compute the cube-coordinate
-- to flat coordinate map.
explore :: HiVal => Set Coord -> Map Loc Coord
explore input = Map.fromList (dfsOn snd step (locOrigin, Set.findMin input))
  where
    step (l, c) =
      [(locRight l, right c) | right c `Set.member` input] ++
      [(locLeft  l, left  c) | left  c `Set.member` input] ++
      [(locUp    l, above c) | above c `Set.member` input] ++
      [(locDown  l, below c) | below c `Set.member` input]

-- | Apply a command to the state of the walker on the cube.
-- Each move is either forward a certain number or a turn.
applyCommand :: HiVal => Map Loc Coord -> Either Int D -> Loc -> Loc
applyCommand maze = \case
  Left  n  -> last . takeWhile (isJust . onMaze maze) . take (n + 1) . iterate locRight
  Right DL -> locRotateR
  Right DR -> locRotateL

-- | Find the location in the input file corresponding to this
-- cube location if one exists.
onMaze :: HiVal => Map Loc Coord -> Loc -> Maybe Coord
onMaze maze = msum . map (`Map.lookup` maze) . take 4 . iterate locRotateR

-- | Find the input file coordinate corresponding to a cube coordinate.
-- The number of clockwise rotations needed to match the orientation
-- from the input file is also returned for using in the password.
locToFlat :: HiVal => Map Loc Coord -> Loc -> (Coord, Int)
locToFlat maze = go 0
  where
    go n loc =
      case Map.lookup loc maze of
        Just c -> (c, n)
        Nothing -> go (n + 1) (locRotateR loc)

-- | Symmetric group S4 corresponds to the symmetries of a cube.
type S4 = Permutation 4

rotX, rotY, rotZ :: S4
rotX = mkPermutation ([3,0,1,2]!!)
rotY = mkPermutation ([2,0,3,1]!!)
rotZ = mkPermutation ([2,3,1,0]!!)

-- | A pair a rotation of a cube face and a position on that face.
data Loc = Loc S4 Coord
  deriving (Show, Ord, Eq)

-- | Initial location on the top-left of a face.
locOrigin :: Loc
locOrigin = Loc mempty origin

locRight, locLeft, locUp, locDown, locRotateL, locRotateR :: HiVal => Loc -> Loc
locRight (Loc p (C y x))
  | x < ?hiVal = Loc p (C y (x + 1))
  | otherwise = Loc (p <> invert rotY) (C y 0)

locLeft (Loc p (C y x))
  | 0 < x = Loc p (C y (x - 1))
  | otherwise = Loc (p <> rotY) (C y ?hiVal)

locDown (Loc p (C y x))
  | y < ?hiVal = Loc p (C (y + 1) x)
  | otherwise = Loc (p <> rotX) (C 0 x)

locUp (Loc p (C y x))
  | 0 < y = Loc p (C (y - 1) x)
  | otherwise = Loc (p <> invert rotX) (C ?hiVal x)

locRotateR (Loc p (C y x)) = Loc (p <> rotZ) (C x (?hiVal - y))

locRotateL (Loc p (C y x)) = Loc (p <> invert rotZ) (C (?hiVal - x) y)
