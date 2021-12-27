{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/3>

-}
{-# Language OverloadedStrings #-} -- for parser
module Main (main) where

import Advent (format, countBy, counts)
import Advent.Coord (Coord(..))
import Data.Map (Map)
import Data.Map qualified as Map

-- | Print the answers to part 1 and 2 of day 3's task.
--
-- >>> :main
-- 115304
-- 275
main :: IO ()
main =
  do let toPatch (i,x,y,sx,sy) = Patch i x y sx sy
     patches <- map toPatch <$> [format|3 (#%u %@ %u,%u: %ux%u%n)*|]
     let fabric = cutFabric patches
     print (part1 fabric)
     print (part2 fabric patches)

-- | Description of a quilt patch.
data Patch = Patch { patchId, offsetX, offsetY, sizeX, sizeY :: !Int }
  deriving (Read, Show)

-- | Given a list of patches, compute the number of patches covering
-- each coordinate.
--
-- >>> cutFabric [Patch {patchId = 3, offsetX = 5, offsetY = 5, sizeX = 2, sizeY = 2}]
-- fromList [(C 5 5,1),(C 5 6,1),(C 6 5,1),(C 6 6,1)]
cutFabric :: [Patch] -> Map Coord Int
cutFabric = counts . concatMap patchCoords

-- | Compute the number of coordinates that are covered by more than one patch
-- given the number of patches that cover each coordinate.
part1 :: Map Coord Int -> Int
part1 = countBy (> 1)

-- | Find the ID of the patch that overlaps with no others given the number
-- of patches that overlap each coordinate and the list of patches.
part2 :: Map Coord Int -> [Patch] -> Int
part2 fabric patches =
  head [ patchId patch
       | patch <- patches
       , all (1 ==) (Map.intersection fabric (cutFabric [patch]))
       ]

-- | Make a set of the coordinates covered by a patch.
patchCoords :: Patch -> [Coord]
patchCoords patch =
  [ C y x
  | x <- [offsetX patch .. offsetX patch + sizeX patch - 1]
  , y <- [offsetY patch .. offsetY patch + sizeY patch - 1]
  ]
