{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/6>

-}
{-# Language OverloadedStrings #-}
module Main (main) where

import Advent        (format, counts)
import Advent.Coord  (Coord(C), cardinal, coordCol, coordRow,
                      above, below, right, left, manhattan, boundingBox)
import Data.List     (groupBy, foldl', sort, sortBy)
import Data.Function (on)
import Data.Ix       (range)
import Data.Map qualified as Map
import Data.Set qualified as Set

-- | Magic number used in part 2 when computing the region that
-- isn't too far from all the points.
far :: Int
far = 10000

-- | Print the answers to day 6
--
-- >>> :main
-- 5365
-- 42513
main :: IO ()
main =
 do let toCoord (x,y) = C y x
    input <- map toCoord <$> [format|6 (%d, %d%n)*|]
    print (part1 input)
    print (part2 input)

-- | Part 1 looks for the largest completely closed region of coordinates
-- that are nearest to one of the input coordinates. We determine that a
-- region is closed by growing the considered region and eliminating
-- any regions that continue to grow. These still growing regions would
-- only grow larger and larger!
part1 ::
  [Coord] {- ^ input coordinates      -} ->
  Int     {- ^ solution               -}
part1 input
  = maximum
  $ Map.mapMaybe id -- eliminate growing regions
  $ Map.intersectionWith match (regionSizes box0) (regionSizes box1)
  where
    regionSizes = counts . concatMap toRegion

    match x y
      | x == y    = Just x
      | otherwise = Nothing

    toRegion c =
      case choices of
        [(r,_)]:_ -> [r] -- only matches on unique minimum
        _         -> []
      where
        choices = groupBy ((==)    `on` snd)
                $ sortBy  (compare `on` snd)
                  [ (coord, manhattan c coord) | coord <- input ]

    Just (topLeft, bottomRight) = boundingBox input

    -- Compute all the coordinates within the min/max bounds as well as a
    -- box that is one larger all the way around
    box0 = range (topLeft, bottomRight)
    box1 = range (left (above topLeft), right (below bottomRight))


-- | Part 2 finds the size of the region with sum of distances less than 10,000
-- by knowing that this region must contain the point found at the median of
-- all x and y coordinates (which is where the distance will be minimized.
-- Because the region is defined by Manhattan distance the region
-- must be connected, so we can find it by expanding this starting point.
-- Next we'll grow the region considering cardinal neighbors for any point that
-- is in bounds. Once we're unable to grow the region any further we return its
-- size.
part2 :: [Coord] -> Int
part2 input = search Set.empty (Set.singleton startingPoint)

  where
    distances :: Coord -> Int
    distances (C y x) = sum (map (manhattan (C y x)) input)

    startingPoint = C (median (map coordRow input)) (median (map coordCol input))

    search seen next =
      case Set.minView next of
        Nothing -> Set.size seen
        Just (i, next')
          | Set.notMember i seen, distances i < far ->
              search (Set.insert i seen)
                     (foldl' (flip Set.insert) next' (cardinal i))
          | otherwise -> search seen next'

-- | Return the median element of a list. For even lists return the second
-- of the two middle elements.
--
-- >>> median [10,1,5]
-- 5
-- >>> median [1,3,4,5]
-- 4
-- >>> median [1,3,9,10,4,5]
-- 5
median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `quot` 2)
