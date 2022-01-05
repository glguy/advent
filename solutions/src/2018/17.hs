{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/17>
-}
module Main (main) where

import Advent (format, countBy)
import Advent.Coord (below, coordCol, coordRow, left, right, Coord(..))
import Data.Array.Unboxed as A
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Print the answers to day 17
--
-- >>> :main
-- 38364
-- 30551
main :: IO ()
main =
 do let toLine (Left (y, xlo, xhi)) = [C y x | x <- [xlo..xhi]]
        toLine (Right (x, ylo, yhi)) = [C y x | y <- [ylo..yhi]]
    input <- concatMap toLine <$> [format|2018 17 (y=%d, x=%d..%d%n|x=%d, y=%d..%d%n)*|]

    let walls = toArray input
        frames = fillSteps walls
        (walls', water) = last frames

    let flowingN  = Set.size water
        standingN = countBy id (zipWith (/=) (A.elems walls) (A.elems walls'))

    print (flowingN + standingN)
    print standingN

-- clay walls and standing water representation ------------------------

-- | Walls are represented with a 'True' value in the array.
type Walls = A.UArray Coord Bool

isWall :: Walls -> Coord -> Bool
isWall walls c = inArray walls c && walls A.! c

-- water flow logic ----------------------------------------------------

-- | Given some initial clay walls, generate the sequence of updated
-- walls (including standing water) and flowing water coordinates.
fillSteps :: Walls -> [(Walls, Set Coord)]
fillSteps walls = (walls, Map.keysSet water)
             : if null fills then [] else fillSteps (walls A.// fills)
  where
    water = waterflow walls

    fills = [(C ly x, True)
               | c@(C ly lx) <- Map.keys water
               , isWall walls (below c)
               , isWall walls (left  c)
               , rightWall <- isContained c
               , x <- [lx .. coordCol rightWall - 1]
               ]

    -- search to the right to see that the bottom extends out to a wall
    isContained c
      | walls A.! c       = [c]
      | walls A.! below c = isContained (right c)
      | otherwise         = []

-- water flow logic ----------------------------------------------------

-- | Water flow mode. This optimization just keeps the water running
-- flat along a surface from trying to turn around back into itself.
data Mode = LookLeft | LookRight | LookDown
  deriving (Eq, Ord, Show)

waterflow :: Walls -> Map Coord Mode
waterflow walls = reachable (waterStep walls) (C startY 500, LookDown)
  where
    startY = coordRow (fst (A.bounds walls))

-- | Given the current walls (including standing water), a water
-- coordinate, and the direction the water is flowing, generate
-- the neighboring water flows.
waterStep :: Walls -> (Coord, Mode) -> [(Coord, Mode)]
waterStep walls (c, mode)
  | not (inArray walls (below c)) = []
  | not (walls A.! below c) = [ (below c, LookDown) ]
  | otherwise = filter (not . isWall walls . fst)
              $ [ (left  c, LookLeft ) | mode /= LookRight ]
             ++ [ (right c, LookRight) | mode /= LookLeft  ]

-- searching -----------------------------------------------------------

-- | Given a function describing neighboring states find all of the
-- reachable state given a starting state. Each state is associated
-- with some metadata that comes from the first time that state was reached.
reachable :: Ord a => ((a,b) -> [(a,b)]) -> (a,b) -> Map a b
reachable next = aux Map.empty
  where
    aux seen (k,v)
      | Map.member k seen = seen
      | otherwise         = foldl' aux (Map.insert k v seen) (next (k,v))

-- array helpers -------------------------------------------------------

-- | Test if an index is contained within an array.
inArray :: (Ix i, IArray a e) => a i e -> i -> Bool
inArray = A.inRange . A.bounds

-- | Convert a list of coordinates into an array marked 'True' for the
-- listed coordinates.
toArray :: [Coord] -> A.UArray Coord Bool
toArray xs = A.accumArray (\_ e -> e) False (C miny minx, C maxy maxx)
                        [ (xy, True) | xy <- xs ]
  where
    miny = minimum (map coordRow xs)
    maxy = maximum (map coordRow xs)
    minx = minimum (map coordCol xs) - 1
    maxx = maximum (map coordCol xs) + 1
