{-# Language BlockArguments, ImportQualifiedPost, BangPatterns #-}
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

import Data.Array.Unboxed (Ix(rangeSize), UArray, accumArray)
import Data.List (foldl', tails)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import Advent (arrIx, getInputMap)
import Advent.Coord (Coord, above, below, boundingBox, left, neighbors, right)

-- |
-- >>> :main
-- 4236
-- 1023
main :: IO ()
main =
 do elves <- Map.keysSet . Map.filter ('#'==) <$> getInputMap 2022 23
    let states = sim elves

    -- part 1
    let round10 = states !! 10
    print case boundingBox round10 of
      Just box -> rangeSize box - Set.size round10
      Nothing  -> 0

    -- part 2
    mapM_ print (sameIx states)

-- | Generate an infinite list of the rounds of the elf movement rules
sim :: Set Coord -> [Set Coord]
sim start = scanl step start moves

-- | Apply a single round of the elf movement rules
step ::
  Set Coord {- ^ initial elf locations -} ->
  (UArray Coord Bool -> Coord -> Maybe Coord) {- ^ proposal rule -} ->
  Set Coord {- ^ final elf locations -}
step elves m = subtractFromSet elves targets <> Map.keysSet targets
   where
      !elves' = coordSet elves
      targets = foldl' updateElf Map.empty elves

      updateElf acc src
        | isCrowded elves' src
        , Just dst <- m elves' src = Map.alter (uniq src) dst acc
        | otherwise = acc

      uniq v Nothing  = Just v  -- If the location is unassigned, assign it
      uniq _ (Just _) = Nothing -- If the location is assigned, unassign it

-- | Predicate testing to see if elf is near any other elf.
isCrowded :: CoordSet -> Coord -> Bool
isCrowded elves elf = any (arrayMember elves) (neighbors elf)

-- | Move directions and their neighbors in the priority order of round 1
moveSets :: [(Coord -> Coord, Coord -> Coord, Coord -> Coord)]
moveSets = [
  (above, left , right),
  (below, left , right),
  (left , above, below),
  (right, above, below)]

moves :: [CoordSet -> Coord -> Maybe Coord]
moves = map (combine . take 4) (tails (cycle moveSets))
  where
    combine [] _ _ = Nothing
    combine ((a,b,c):xs) elves here
      | not (any (arrayMember elves) locs) = Just here'
      | otherwise = combine xs elves here
      where
        here' = a here
        locs = [here', b here', c here']

-- List utilites

-- | Find index in list where element is the same as the previous element.
sameIx :: Ord a => [a] -> Maybe Int
sameIx = go 1
  where
    go !i (x:y:z)
      | x == y    = Just i
      | otherwise = go (i+1) (y:z)
    go _ _ = Nothing

-- Set utilities

subtractFromSet :: (Foldable f, Ord a) => Set a -> f a -> Set a
subtractFromSet = foldl' (flip Set.delete)

-- | A set of coordinates represented with an array for fast membership lookups
type CoordSet = UArray Coord Bool

-- | Build an array-based representation of the set of coordinates. Because our elves are
-- densely packed, and we do lots of membership tests, this representation is more efficient.
coordSet :: Set Coord -> CoordSet
coordSet s = accumArray (\_old new -> new) False b [(c, True) | c <- Set.toList s]
  where
    b = fromMaybe (0,0) (boundingBox s)

-- | Membership operation for CoordSet
arrayMember :: CoordSet -> Coord -> Bool
arrayMember a x = fromMaybe False (arrIx a x)
