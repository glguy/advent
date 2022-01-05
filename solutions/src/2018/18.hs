{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/18>
-}
module Main (main) where

import Advent (getInputArray, count)
import Advent.Coord (Coord(C), neighbors)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Array.Unboxed qualified as A

-- | Represent the lumber area as an array of characters: @.|#@
type Area = A.UArray Coord Char

-- | Print the answers to day 18
--
-- >>> :main
-- 506160
-- 189168
main :: IO ()
main =
  do input <- getInputArray 2018 18
     print (part1 input)
     print (part2 input)

-- | Compute the resource value after 10 minutes
part1 :: Area -> Int
part1 input = resourceValue (timesteps 10 input)

-- | Compute the resource value after 1,000,000,000 minutes.
-- This implementation relies on the lumber area entering a cycle.
-- Once the cycle is detected with can figure out how to advance
-- forward to near the end of the target duration.
part2 :: Area -> Int
part2 input = resourceValue (timesteps cleanup x)
  where
    (starti, repeati, x) = findCycle Map.empty input

    target    = 1000000000
    period    = repeati - starti
    remaining = target - repeati
    cleanup   = remaining `rem` period

-- | Given a map of past areas and the current area, find the two
-- minutes that have the same state and return the state of the
-- area at those minutes.
findCycle :: Map Area Int -> Area -> (Int, Int, Area)
findCycle seen x =
  case Map.lookup x seen of
    Just prev -> (prev, Map.size seen, x)
    Nothing   -> findCycle (Map.insert x (Map.size seen) seen) (timestep x)

-- | Multiply the number of trees in the area by the number of lumberyards.
resourceValue :: Area -> Int
resourceValue v = count '|' xs * count '#' xs
  where
    xs = A.elems v

-- | Linearly apply the 'timestep' function a given number of times
timesteps :: Int -> Area -> Area
timesteps 0 v = v
timesteps n v = timesteps (n-1) $! timestep v

-- | Update the lumber area for one minute passing
timestep :: Area -> Area
timestep v = imap cell v
  where
    test c i  = inArray v i && c == v A.! i

    cell i '.' | atLeast 3 (test '|') (neighbors i) = '|'
    cell i '|' | atLeast 3 (test '#') (neighbors i) = '#'
    cell i '#' | not (any (test '#') (neighbors i))
              || not (any (test '|') (neighbors i)) = '.'
    cell _ c = c

-- | Determine if at least a given number of elements satisfy a predicate
atLeast :: Int -> (a -> Bool) -> [a] -> Bool
atLeast 0 _ _      = True
atLeast _ _ []     = False
atLeast n p (x:xs) = atLeast (if p x then n-1 else n) p xs

-- | Test if an index is contained within an array.
inArray :: (A.Ix i, A.IArray a e) => a i e -> i -> Bool
inArray = A.inRange . A.bounds

-- | Map a function over the indexes and elements of an array.
imap :: (A.Ix i, A.IArray a e) => (i -> e -> e) -> a i e -> a i e
imap f a = A.listArray (A.bounds a) [ f i e | (i, e) <- A.assocs a ]
