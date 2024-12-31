{-# Language QuasiQuotes, ConstraintKinds, TemplateHaskell, ImportQualifiedPost, LambdaCase, ImplicitParams, DataKinds #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2022
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
module Main where

import Advent (stageTH, format, countBy)
import Advent.Coord (Coord(..), coordLines, above, below, left, origin, right)
import Advent.Permutation (Permutation, mkPermutation, invert)
import Advent.Search (dfsOn)
import Control.Monad (msum)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import qualified Advent.AsmProg as cube

data D = DL | DR

stageTH

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
    let S endLoc endFacing = fixFacing maze (foldl (applyCommand maze) (S originLoc 0) path)
    
    -- translate the cube coordinates back into flat coordinates
    let C y x = maze Map.! endLoc

    -- compute the "password" from the end location
    print (1000 * (y + 1) + 4 * (x + 1) + endFacing)

-- | Given the set of flat path coordinates compute the cube-coordinate
-- to flat coordinate map.
explore :: HiVal => Set Coord -> Map Loc Coord
explore input = Map.fromList (dfsOn snd step (originLoc, Set.findMin input))
  where
    step (l, c) =
      [(locRight l, right c) | right c `Set.member` input] ++
      [(locLeft  l, left  c) | left  c `Set.member` input] ++
      [(locUp    l, above c) | above c `Set.member` input] ++
      [(locDown  l, below c) | below c `Set.member` input]

-- | A location on the cube and a direction
data S = S !Loc !Facing

-- | Apply a command to the state of the walker on the cube.
-- Each move is either forward a certain number or a turn.
applyCommand :: HiVal => Map Loc Coord -> S -> Either Int D -> S
applyCommand maze (S here dir) = \case
  Left  n -> S (walkN maze n dir here) dir
  Right t -> S here (turn t dir)

-- | Walk a number of steps in the given direction
walkN :: HiVal => Map Loc Coord -> Int -> Facing -> Loc -> Loc
walkN maze n dir here = last (takeWhile valid (take (n + 1) (iterate (move dir) here)))
  where valid = isJust . onMaze maze

-- | Find the location in the input file corresponding to this
-- cube location if one exists.
onMaze :: HiVal => Map Loc Coord -> Loc -> Maybe Coord
onMaze maze loc = msum (map (`Map.lookup` maze) (take 4 (iterate locRotate loc)))

-- | Symmetric group S4 corresponds to the symmetries of a cube.
type S4 = Permutation 4

rotX, rotY, rotZ :: S4
rotX = mkPermutation ([3,0,1,2]!!)
rotY = mkPermutation ([2,0,3,1]!!)
rotZ = mkPermutation ([2,3,1,0]!!)

-- | A location is a cube-face and rotation paired with a location on that face
data Loc = Loc { locFace :: S4, locCoord :: Coord }
  deriving (Show, Ord, Eq)

-- | Initial location on the top-left or a face.
originLoc :: Loc
originLoc = Loc mempty origin

locRight, locLeft, locUp, locDown, locRotate :: HiVal => Loc -> Loc
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

-- Rotate the representation of the current location 90-degrees
-- clockwise in order to put it onto a symmetric cube-face.
locRotate (Loc p (C y x)) = Loc (p <> rotZ) (C x (?hiVal - y))

-- | Rotate the facing until we're on the cube face as it
-- is oriented on the input text.
fixFacing :: HiVal => Map Loc Coord -> S -> S
fixFacing maze (S loc n)
  | Map.member loc maze = S loc n
  | otherwise = fixFacing maze (S (locRotate loc) (turn DR n))

type Facing = Int

turn :: D -> Facing -> Facing
turn DL x = (x - 1) `mod` 4
turn DR x = (x + 1) `mod` 4

move :: HiVal => Facing -> Loc -> Loc
move 0 = locRight
move 1 = locDown
move 2 = locLeft
move 3 = locUp
move _ = error "move: bad facing"
