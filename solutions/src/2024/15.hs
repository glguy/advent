{-# Language QuasiQuotes, LambdaCase, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/15>

>>> :{
:main + "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########
\&
<^^>>>vv<v>>v<<
"
:}
2028
1751

>>> :{
:main + "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######
\&
<vv<<^^<<^^
"
:}
908
618

>>> :{
:main + "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########
\&
<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
"
:}
10092
9021

-}
module Main (main) where

import Advent (format)
import Advent.Coord (Coord(..), charToVec, coordLines, coordRow, east, west)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

-- | >>> :main
-- 1499739
-- 1522215
main :: IO ()
main =
 do (input1, input2) <- [format|2024 15 (%s%n)*%n(%s%n)*|]
    let grid = buildGrid input1
    let start1:_ = [p | (p, '@') <- Map.assocs grid]
    let dirs = mapMaybe charToVec (concat input2)
    print (score (fst (foldl sim (grid, start1) dirs)))

    let grid2 = buildGrid (map (concatMap expandCell) input1)
    let start2:_ = [p  | (p, '@') <- Map.assocs grid2]
    print (score (fst (foldl sim (grid2, start2) dirs)))

buildGrid :: [String] -> Map Coord Char
buildGrid = Map.fromList . filter (\x -> snd x /= '.') . coordLines

expandCell :: Char -> String
expandCell = \case
    '\n' -> "\n"
    '#'  -> "##"
    'O'  -> "[]"
    '.'  -> ".."
    '@'  -> "@."
    _    -> error "bad input"

score :: Map Coord Char -> Int
score m = sum [100 * y + x | (C y x, c) <- Map.assocs m, c == 'O' || c == '[']

sim :: (Map Coord Char, Coord) -> Coord -> (Map Coord Char, Coord)
sim (grid, start) d =
    case go Map.empty [start] of
      Nothing     -> (grid, start)
      Just region -> (grid', start + d)
        where
          grid' = Map.union (Map.mapKeysMonotonic (d +) region)
                            (Map.difference grid region)
  where
    vertical = coordRow d /= 0

    go seen [] = Just seen
    go seen (x:xs)
      | Map.notMember x seen
      , Just c <- Map.lookup x grid
      = if c == '#' then Nothing else
        let next = case c of
              '[' -> [x + east | vertical] ++ [x + d]
              ']' -> [x + west | vertical] ++ [x + d]
              'O' ->                          [x + d]
              '@' ->                          [x + d]
              _   -> []
        in go (Map.insert x c seen) (next ++ xs)
      | otherwise = go seen xs
