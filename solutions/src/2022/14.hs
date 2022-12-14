{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/14>

-}
module Main where

import Control.Monad (foldM)
import Data.List (find, foldl')
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Functor.Identity

import Advent (format)
import Advent.Coord (below, coordRow, left, right, Coord(..))

-- |
-- >>> :main
-- 644
-- 27324
main :: IO ()
main =
 do input <- [format|2022 14 ((%u,%u)&( -> )%n)*|]
    let world = Set.fromList (concatMap segs input)
        limit = 1 + maximum [ y| C y _ <- Set.toList world]

    case fillFrom Left limit world top of
      Right {}    -> fail "no solution"
      Left world1 -> print (Set.size world1 - Set.size world)

    case fillFrom Identity limit world top of
      Identity world2 -> print (Set.size world2 - Set.size world)

-- | The entry point of sand at @500,0@
top :: Coord
top = C 0 500

fillFrom :: Monad m => (Set Coord -> m (Set Coord)) -> Int -> Set Coord -> Coord -> m (Set Coord)
fillFrom onVoid limit world here
  | limit < coordRow here = onVoid world
  | Set.member here world = pure world
  | otherwise = Set.insert here <$> foldM (fillFrom onVoid limit) world
                  [below here, left (below here), right (below here)]

-- Turning line segments into sets of coordinates

segs :: [(Int,Int)] -> [Coord]
segs (x:y:z) = seg x y ++ segs (y:z)
segs [(x,y)] = [C y x]
segs []      = []

seg :: (Int,Int) -> (Int,Int) -> [Coord]
seg (a,b) (c,d)
  | a == c    = [C y a | y <- [min b d .. max b d]]
  | b == d    = [C d x | x <- [min a c .. max a c]]
  | otherwise = error "unexpected input"
