{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/23>

>>> :{
:main +
  ".......#......\n\
  \.....###.#....\n\
  \...#...#.#....\n\
  \....#...##....\n\
  \...#.###......\n\
  \...##.#.##....\n\
  \....#..#......\n"
:}
110
20

-}
module Main where

import Data.List (tails)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Array.Unboxed (Ix(rangeSize), UArray, accumArray)
import Advent (getInputMap, counts, arrIx)
import Advent.Coord (Coord, above, below, boundingBox, left, neighbors, right)

-- |
-- >>> :main
-- 4236
-- 1023
main :: IO ()
main =
 do input <- Map.keysSet . Map.filter ('#'==) <$> getInputMap 2022 23
    let states = sim input

    -- part 1
    let b = states !! 10
    print case boundingBox b of
      Just box -> rangeSize box - Set.size b
      Nothing  -> 0

    -- part 2
    print (sameIx 1 states)

sameIx :: Int -> [Set Coord] -> Int
sameIx i (x:y:z)
  | x == y = i
  | otherwise = sameIx (i+1) (y:z)
sameIx _ _ = undefined

sim :: Set Coord -> [Set Coord]
sim start = scanl step start moves

step :: Set Coord -> (UArray Coord Bool -> Coord -> Maybe Coord) -> Set Coord
step elves m = elves Set.\\ moved <> Map.keysSet ok
   where
      elves'  = setArray elves
      targets = Map.fromList
                  [ (elf, c)
                  | elf <- Set.toList elves
                  , isCrowded elves' elf
                  , c <- maybeToList (m elves' elf)
                  ]
      ok      = Map.filter (1 ==) (counts targets)
      moved   = Map.keysSet (Map.filter (`Map.member` ok) targets)

setArray :: Set Coord -> UArray Coord Bool
setArray s = accumArray (\_ x -> x) False b [(c, True) | c <- Set.toList s]
  where
    b = fromMaybe (0,0) (boundingBox s)

arrayMember :: UArray Coord Bool -> Coord -> Bool
arrayMember a x = fromMaybe False (arrIx a x)

isCrowded :: UArray Coord Bool -> Coord -> Bool
isCrowded elves elf = any (arrayMember elves) (neighbors elf)

moveSets :: [(Coord -> Coord, Coord -> Coord, Coord -> Coord)]
moveSets = [
  (above, left , right),
  (below, left , right),
  (left , above, below),
  (right, above, below)]

moves :: [UArray Coord Bool -> Coord -> Maybe Coord]
moves = map (combine . take 4) (tails (cycle moveSets))
  where
    combine [] _ _ = Nothing
    combine ((a,b,c):xs) elves here
      | not (any (arrayMember elves) locs) = Just (a here)
      | otherwise = combine xs elves here
      where
        here' = a here
        locs = [here', b here', c here']
