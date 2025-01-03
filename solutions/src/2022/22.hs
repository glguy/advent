{-# Language QuasiQuotes, BangPatterns, ConstraintKinds, TemplateHaskell, ImportQualifiedPost, LambdaCase, ImplicitParams, DataKinds #-}
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
6032
5031

-}
module Main (main) where

import Advent (stageTH, format)
import Advent.Coord (Coord(..), coordLines, above, below, left, right, east, turnLeft, turnRight, mapCoord)
import Advent.Permutation (Permutation, mkPermutation, invert)
import Advent.Search (dfsOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Turn commands.
data D
  = DL -- ^ Left turn
  | DR -- ^ Right turn

-- | Tile in the maze.
data T
  = T_HASH -- ^ A wall
  | T_DOT  -- ^ A path
  deriving (Eq, Ord, Show)

stageTH

-- | Constraint for upper-bound of cube coordinates
type Hi = ?hi :: Int

-- |
-- >>> :main
-- 162186
-- 55267
main :: IO ()
main =
 do (rawMaze, cmds) <- [format|2022 22 (( |@T)*%n)*%n(%u|@D)*%n|]
    let maze  = parseMaze rawMaze
        start = pickStart rawMaze

    print (part1 start maze cmds)
    print (part2 start maze cmds)

-- | Build a flat-coordinate map of the maze cells from the lines of
-- raw tiles.
parseMaze :: [[Maybe T]] -> Map Coord T
parseMaze cs = Map.fromList [(c, x) | (c, Just x) <- coordLines cs]

-- | You begin the path in the leftmost open tile of the top row of tiles.
pickStart :: [[Maybe T]] -> Coord
pickStart rawMaze =
  case [c | (c, Just T_DOT) <- coordLines (take 1 rawMaze)] of
    c : _ -> c
    _ -> error "No acceptable starting location"

-- | The final password is the sum of 1000 times the row, 4 times the
-- column, and the facing.
password :: Coord -> Int -> Int
password (C y x) z = 1000 * (y + 1) + 4 * (x + 1) + z

-- | Follow the command sequence while using a flat wrap-around
-- logic to compute the password.
part1 :: Coord -> Map Coord T -> [Either Int D] -> Int
part1 start maze cmds = password end (d `mod` 4)
  where
    (end, _, d) = foldl (applyCommand1 maze) (start, east, 0) cmds

-- | Update the current location, direction and facing given a single
-- turn or move command.
applyCommand1 :: Map Coord T -> (Coord, Coord, Int) -> Either Int D -> (Coord, Coord, Int)
applyCommand1 board (!here, !dir, !facing) = \case
  Right DL -> (here, turnLeft  dir, facing - 1)
  Right DR -> (here, turnRight dir, facing + 1)
  Left  n  -> (here', dir, facing)
    where
      here' = last (takeWhile isOpen (take (n + 1) (iterate step here)))
      isOpen x = board Map.! x == T_DOT
      step x
        | let x' = x + dir, Map.member x' board = x'
        | otherwise = last (takeWhile (`Map.member` board) (iterate (subtract dir) x))

-- | Follow the command sequence while treating the maze as a cube net
-- to compute the resulting password.
part2 :: Coord -> Map Coord T -> [Either Int D] -> Int
part2 start maze cmds =
 do -- figure out the side-length of the cube we're working with
    -- so that we can handle both examples and regular inputs
    let ?hi = until (\x -> 6 * x * x >= length maze) (1 +) 1 - 1

    -- associate cube coordinates with all of the input file coordinates
    let (start', cube) = buildCube start (Map.keysSet (Map.filter (T_DOT ==) maze))

    -- figure out the cube coordinate that our path ends on
    let (end, facing) = cube Map.! foldl (flip (applyCommand2 cube)) start' cmds

    -- compute the "password" from the end location
    password end facing

-- | Given the set of flat path coordinates compute the cube-coordinate
-- to flat-coordinate and facing map. The cube location of the maze
-- starting position is returned.
buildCube :: Hi => Coord -> Set Coord -> (Loc, Map Loc (Coord, Int))
buildCube start input = (start', cube)
  where
    start' = Loc mempty (mapCoord (`mod` (?hi + 1)) start)
    cube = Map.fromList
            [(li, (c, i)) | (l, c) <- dfsOn snd step (start', start)
                          , (li, i) <- zip (iterate locRotateL l) [0..3]]

    -- advance the cube location and the flat location in lock-step to
    -- create the map of location correspondences
    step (l, c) =
      [(locRight l, right c) | right c `Set.member` input] ++
      [(locLeft  l, left  c) | left  c `Set.member` input] ++
      [(locUp    l, above c) | above c `Set.member` input] ++
      [(locDown  l, below c) | below c `Set.member` input]

-- | Apply a command to the state of the walker on the cube.
-- Each move is either forward a certain number or a turn.
applyCommand2 :: Hi => Map Loc a -> Either Int D -> Loc -> Loc
applyCommand2 maze = \case
  Left  n  -> last . takeWhile (`Map.member` maze) . take (n + 1) . iterate locRight
  Right DL -> locRotateR
  Right DR -> locRotateL

-- | Symmetric group S4 corresponds to the symmetries of a cube.
--
-- This cube's diagonals are labeled and the face is read off the
-- top face clockwise. Rotations about an axis use left-hand rule.
--
-- @
--   0--1   z
--  /| /|   |
-- 3--2 |   o-x
-- | 2|-3  /
-- |/ |/  y
-- 1--0
-- @
type S4 = Permutation 4

rotX, rotY, rotZ :: S4
rotX = mkPermutation ([3,2,0,1] !!)
rotY = mkPermutation ([2,0,3,1] !!)
rotZ = mkPermutation ([3,0,1,2] !!)

-- | A pair of a cube orientation and a position on the top face.
data Loc = Loc S4 Coord
  deriving (Show, Ord, Eq)

locRight, locLeft, locUp, locDown, locRotateL, locRotateR :: Hi => Loc -> Loc
locRight (Loc p (C y x))
  | x < ?hi = Loc p (C y (x + 1))
  | otherwise = Loc (p <> invert rotY) (C y 0)

locLeft (Loc p (C y x))
  | 0 < x = Loc p (C y (x - 1))
  | otherwise = Loc (p <> rotY) (C y ?hi)

locDown (Loc p (C y x))
  | y < ?hi = Loc p (C (y + 1) x)
  | otherwise = Loc (p <> rotX) (C 0 x)

locUp (Loc p (C y x))
  | 0 < y = Loc p (C (y - 1) x)
  | otherwise = Loc (p <> invert rotX) (C ?hi x)

locRotateR (Loc p (C y x)) = Loc (p <> rotZ) (C x (?hi - y))

locRotateL (Loc p (C y x)) = Loc (p <> invert rotZ) (C (?hi - x) y)
