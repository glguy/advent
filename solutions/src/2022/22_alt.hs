{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost, LambdaCase, BangPatterns, DataKinds #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/22>

-}
module Main where

import Advent (stageTH, format)
import Advent.Coord (Coord(..), coordLines, above, below, left, origin, right)
import Advent.Permutation (Permutation, mkPermutation, invert)
import Advent.Search ( dfsOn )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Monad (msum)
import Data.Maybe (isJust)

data D = DL | DR

stageTH

-- | Largest coordinate on a cube face named to make it easier to check
-- examples.
highVal :: Int
highVal = 49

-- |
-- >>> :main
-- 55267
main :: IO ()
main =
 do (rawmap, path) <- [format|2022 22 (( |.|#)*!%n)*%n(%u|@D)*%n|]
    let maze = explore (Set.fromList [c | (c, '.') <- coordLines rawmap])
        (endLoc, endFacing) = foldl (applyCommand maze) (originLoc, 0) path
        Just (C y x) = onMaze maze endLoc
        endFacing' = fixFacing maze endLoc endFacing
    print (1000 * (y + 1) + 4 * (x + 1) + endFacing')

-- | Given the set of flat path coordinates compute the cube-coordinate
-- to flat coordinate map.
explore :: Set Coord -> Map Loc Coord
explore input = Map.fromList (dfsOn snd step (originLoc, Set.findMin input))
  where
    step (l, c) =
      [(locRight l, right c) | right c `Set.member` input] ++
      [(locLeft  l, left  c) | left  c `Set.member` input] ++
      [(locUp    l, above c) | above c `Set.member` input] ++
      [(locDown  l, below c) | below c `Set.member` input]

applyCommand :: Map Loc Coord -> (Loc, Facing) -> Either Int D -> (Loc, Facing)
applyCommand maze (!here, !dir) = \case
  Left  n -> (walkN maze n dir here, dir)
  Right t -> (here, turn t dir)

-- | Walk a number of steps in the given direction
walkN :: Map Loc Coord -> Int -> Facing -> Loc -> Loc
walkN maze n dir here = last (takeWhile valid (take (n + 1) (iterate (move dir) here)))
  where valid = isJust . onMaze maze

-- | Find the location in the input file corresponding to this
-- cube location if one exists.
onMaze :: Map Loc Coord -> Loc -> Maybe Coord
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

originLoc :: Loc
originLoc = Loc mempty origin

locRight :: Loc -> Loc
locRight (Loc p (C y x))
  | x < highVal = Loc p (C y (x + 1))
  | otherwise = Loc (p <> invert rotY) (C y 0)

locLeft :: Loc -> Loc
locLeft (Loc p (C y x))
  | 0 < x = Loc p (C y (x - 1))
  | otherwise = Loc (p <> rotY) (C y highVal)

locDown :: Loc -> Loc
locDown (Loc p (C y x))
  | y < highVal = Loc p (C (y + 1) x)
  | otherwise = Loc (p <> rotX) (C 0 x)

locUp :: Loc -> Loc
locUp (Loc p (C y x))
  | 0 < y = Loc p (C (y - 1) x)
  | otherwise = Loc (p <> invert rotX) (C highVal x)

locRotate :: Loc -> Loc
locRotate (Loc p (C y x)) = Loc (p <> rotZ) (C x (highVal - y))

-- | Rotate the facing until we're on the cube face as it
-- is oriented on the input text.
fixFacing :: Map Loc Coord -> Loc -> Facing -> Facing
fixFacing maze loc n
  | Map.member loc maze = n
  | otherwise = fixFacing maze (locRotate loc) (turn DR n)

type Facing = Int

turn :: D -> Facing -> Facing
turn DL x = (x - 1) `mod` 4
turn DR x = (x + 1) `mod` 4

move :: Facing -> Loc -> Loc
move 0 = locRight
move 1 = locDown
move 2 = locLeft
move 3 = locUp
move _ = error "move: bad facing"
