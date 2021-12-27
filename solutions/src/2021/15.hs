{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/15>

Finding the shortest route through a cave, and then
finding the shortest route through a slightly larger cave.

This solution uses Djikstra's Algorithm to perform a
shortest path search through the cave. (A* with a zero
heuristic degenerates to this)

For part 2 this solution transforms the lookup coordinates
rather than to build a larger cave array. The reason for this
is to reduce memory pressure especially when running the
search on much larger maps.

-}
module Main (main) where

import Advent (arrIx, getInputArray)
import Advent.Coord (Coord(..), cardinal, origin)
import Advent.Search (AStep(..), astar)
import Data.Array.Unboxed ((!), amap, IArray(bounds), UArray)
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Word (Word8)

-- | >>> :main
-- 698
-- 3022
main :: IO ()
main =
 do inp <- amap (fromIntegral . digitToInt) <$> getInputArray 15
    let end = snd (bounds inp)
    print (solve end (arrIx inp))
    let (end2, inp2) = extendCave inp
    print (solve end2 inp2)

-- | Compute the risk score traveling through a cave.
solve :: Coord -> (Coord -> Maybe Word8) -> Int
solve end m = fromMaybe (error "no path") (lookup end costs)
  where
    costs = astar step origin
    step here = [ AStep next (fromIntegral cost) 0
                | next <- cardinal here
                , cost <- maybeToList (m next)]

-- | Build a larger cave by tiling the input cave in a 5x5
-- grid. Added caves have their risk values updated according
-- to their new locations.
extendCave :: UArray Coord Word8 -> (Coord, Coord -> Maybe Word8)
extendCave m = end `seq` (end, f)
  where
    (C 0 0, end0@(C hiy hix)) = bounds m
    wy  = 1+hiy
    wx  = 1+hix
    end = C (4*wy) (4*wx) + end0
    f (C y x) =
      case (divMod y wy, divMod x wx) of
        ((ty, y'),(tx,x'))
          | 0 <= ty, ty <= 4, 0 <= tx, tx <= 4 ->
            Just $! fixRisk (m ! C y' x' + fromIntegral (tx + ty))
        _ -> Nothing

-- | Risks are defined to roll over from 9 back to 1
--
-- >>> fixRisk <$> [1,5,9,10,12]
-- [1,5,9,1,3]
fixRisk :: Word8 -> Word8
fixRisk x = (x - 1) `mod` 9 + 1
