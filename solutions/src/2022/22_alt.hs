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
import Advent.Permutation as P
import Advent.Search ( dfsOn )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

data D = DL | DR

stageTH

-- |
-- >>> :main
-- 55267
main :: IO ()
main =
 do (rawmap, path) <- [format|2022 22 (( |.|#)*!%n)*%n(%u|@D)*%n|]
    let maze = explore (Set.fromList [c | (c, '.') <- coordLines rawmap])
    let (endLoc, endFacing) = foldl (applyCommand maze) (originLoc, 0) path
        endFacing' = fixFacing (locFace endLoc) endFacing
        C y x = maze Map.! normalizeLoc endLoc
    print (1000 * (y + 1) + 4 * (x + 1) + endFacing')

applyCommand :: Map Loc Coord -> (Loc, Facing) -> Either Int D -> (Loc, Facing)
applyCommand maze (!here, !dir) = \case
  Left  n -> (walkN n dir here maze, dir)
  Right t -> (here, turn t dir)

walkN :: Int -> Facing -> Loc -> Map Loc Coord -> Loc
walkN n dir here board
  | let here' = move dir here, n > 0, normalizeLoc here' `Map.member` board = walkN (n - 1) dir here' board
  | otherwise = here

type S4 = Permutation 4

-- X -->
-- Y v
-- Z up
-- lefthand rule curls clockwise

rotX, rotY, rotZ :: S4
rotX = mkPermutation ([3,0,1,2]!!)
rotY = mkPermutation ([2,0,3,1]!!)
rotZ = mkPermutation ([2,3,1,0]!!)

-- | A location is a cube-face and rotation paired with a location on that face
data Loc = Loc { locFace :: Permutation 4, locCoord :: Coord }
  deriving (Show, Ord, Eq)

locRight :: Loc -> Loc
locRight (Loc p (C y x))
  | x < 49 = Loc p (C y (x + 1))
  | otherwise = Loc (p <> P.invert rotY) (C y 0)

locLeft :: Loc -> Loc
locLeft (Loc p (C y x))
  | 0 < x = Loc p (C y (x - 1))
  | otherwise = Loc (p <> rotY) (C y 49)

locDown :: Loc -> Loc
locDown (Loc p (C y x))
  | y < 49 = Loc p (C (y + 1) x)
  | otherwise = Loc (p <> rotX) (C 0 x)

locUp :: Loc -> Loc
locUp (Loc p (C y x))
  | 0 < y = Loc p (C (y - 1) x)
  | otherwise = Loc (p <> P.invert rotX) (C 49 x)

normalizeLoc :: Loc -> Loc
normalizeLoc (Loc p (C y x))
  | P.index p 0 == 0 = Loc p (C y x)
  | otherwise = normalizeLoc (Loc (p <> rotZ) (C x (49 - y)))

fixFacing :: S4 -> Facing -> Facing
fixFacing p n
  | P.index p 0 == 0 = n `mod` 4
  | otherwise = fixFacing (p <> rotZ) (n-1)

originLoc :: Loc
originLoc = Loc mempty origin

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

explore :: Set Coord -> Map Loc Coord
explore input = Map.fromList
  [(normalizeLoc l, c) | (l, c) <- dfsOn snd step (originLoc, Set.findMin input)]
  where
    step (l, c) =
      [(locRight l, right c) | right c `Set.member` input] ++
      [(locLeft  l, left  c) | left  c `Set.member` input] ++
      [(locUp    l, above c) | above c `Set.member` input] ++
      [(locDown  l, below c) | below c `Set.member` input]