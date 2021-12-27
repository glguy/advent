{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/24>

-}
module Main (main) where

import Advent (getInputLines)
import Advent.Coord (Coord(..), coordLines, cardinal)
import Advent.Coord3 (Coord3(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map.Strict qualified as Map

-- | >>> :main
-- 12531574
-- 2033
main :: IO ()
main =
  do inp <- bugCoords <$> getInputLines 24
     print (biodiversity (findDup (iterate (update (filter inside . cardinal)) inp)))

     let inp3 = Set.map to3 inp
     print (Set.size (iterate (update cardinal3) inp3 !! 200))

-- | Compute the part 1 biodiversity score.
biodiversity :: Set Coord -> Int
biodiversity m = sum [ 2^(12+(y*5)+x) | C y x <- Set.toList m]

-- | Apply automata rule to set of bug coordinates.
update :: Ord a => (a -> [a]) -> Set a -> Set a
update adjacents m
  = Set.filter rule
  $ Set.union m
  $ Map.keysSet density
  where
    rule k = 1 == n || 2 == n && Set.notMember k m
      where n = Map.findWithDefault 0 k density

    density = Map.fromListWith (+)
              [(d, 1::Int) | c <- Set.toList m, d <- adjacents c]

-- | Compute the coordinates of the input bugs centered around 0,0
bugCoords :: [String] -> Set Coord
bugCoords xs = Set.fromList [k - 2 | (k, '#') <- coordLines xs]

-- | Find the first duplicate element in a list.
findDup :: Ord a => [a] -> a
findDup = go Set.empty
  where
    go _    [] = error "no duplicates"
    go seen (x:xs)
       | Set.member x seen = x
       | otherwise         = go (Set.insert x seen) xs

-- | Check that a coordinate is contained within the 5x5 region centered
-- around the origin.
inside :: Coord -> Bool
inside (C y x) = abs x <= 2 && abs y <= 2

------------------------------------------------------------------------
-- 3-dimensional recursive board coordinates
------------------------------------------------------------------------

to3 :: Coord -> Coord3
to3 (C y x) = C3 0 y x

toL, toR, toA :: Coord3 -> Coord3
toL (C3 d y x) = C3 d (-x) y
toR (C3 d y x) = C3 d x (-y)
toA (C3 d y x) = C3 d (-y) (-x)

cardinal3 :: Coord3 -> [Coord3]
cardinal3 c =
  concat [          above3       c,
          map toR $ above3 $ toL c,
          map toL $ above3 $ toR c,
          map toA $ above3 $ toA c]

{-# INLINE above3 #-}
above3 :: Coord3 -> [Coord3]
above3 (C3 d 1    0) = [C3 (d+1) (  2) x| x <- [-2..2]]
above3 (C3 d (-2) _) = [C3 (d-1) ( -1) 0]
above3 (C3 d y    x) = [C3 (d  ) (y-1) x]
