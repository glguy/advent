{-# Language ViewPatterns #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<http://adventofcode.com/2017/day/21>

Day 21 defines a system of rewrite rules on a grid of points that are
applied to 2x2 or 3x3 subtiles of the whole grid.

>>> let inputFile = unlines ["../.# => ##./#../...",".#./..#/### => #..#/..../..../#..#"]
>>> let rules = makeRules (parseInput inputFile)
>>> let iterations = iterate (mapSubSquares rules) start

>>> printGrid (iterations !! 0)
.#.
..#
###

>>> printGrid (iterations !! 1)
#..#
....
....
#..#

>>> printGrid (iterations !! 2)
##.##.
#..#..
......
##.##.
#..#..
......

-}
module Main where

import           Advent
import           Data.List
import           Data.List.Split
import qualified Data.Map as Map

-- $setup
-- >>> let printGrid = mapM_ putStrLn

-- | Print the number of active grid cells after 5 and 18 iterations.
-- The input file can be overridden via command-line arguments.
main :: IO ()
main =
  do input <- parseInput <$> getInputLines 21

     let rules      = makeRules input
         iterations = iterate (mapSubSquares rules) start

     print (countCells (iterations !!  5))
     print (countCells (iterations !! 18))


type Grid = [[Char]]


-- | Initial grid value (a game of life glider).
--
-- >>> printGrid start
-- .#.
-- ..#
-- ###
start :: Grid
start = [".#.", "..#", "###"]


-- | Count the number of cells set in a grid.
--
-- >>> countCells start
-- 5
countCells :: Grid -> Int
countCells = count '#' . concat


-- | Generate all of the rotated and flipped versions of a grid.
--
-- >>> printGrid (intercalate "  " <$> transpose (similarSquares start))
-- .#.  .##  ###  #..  ###  ##.  .#.  ..#
-- ..#  #.#  #..  #.#  ..#  #.#  #..  #.#
-- ###  ..#  .#.  ##.  .#.  #..  ###  .##
similarSquares :: Grid -> [Grid]
similarSquares x = concatMap (take 4 . iterate rotateCCW) [x, reverse x]


-- | Rotate a grid counter-clockwise.
--
-- >>> printGrid (rotateCCW start)
-- .##
-- #.#
-- ..#
rotateCCW :: Grid -> Grid
rotateCCW = reverse . transpose


-- | Apply a function to all of the subsquares of a grid.
mapSubSquares :: (Grid -> Grid) -> Grid -> Grid
mapSubSquares rules xs =
  concatMap
    (map concat . transpose . map rules . transpose . map (chunksOf n))
    (chunksOf n xs)
  where
    n | even (length xs) = 2
      | otherwise        = 3


-- | Build the grid update function given the list of rules
-- loaded from the input file.
makeRules :: [(Grid,Grid)] -> Grid -> Grid
makeRules rs =
  let rulesMap = Map.fromList [ (k',v) | (k,v) <- rs , k' <- similarSquares k ]
  in (rulesMap Map.!)


-- | Parse a string a list of grid rules.
parseInput :: [String] -> [(Grid,Grid)]
parseInput = map parseRule


-- | Parse a string as a rule mapping one grid to another.
--
-- >>> parseRule "../.# => ##./#../..."
-- (["..",".#"],["##.","#..","..."])
parseRule :: String -> (Grid,Grid)
parseRule (words -> [a,"=>",b]) = (splitOn "/" a, splitOn "/" b)
parseRule x = error ("bad input: " ++ x)
