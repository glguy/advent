{-# Language QuasiQuotes, BlockArguments, LambdaCase, ImportQualifiedPost, TransformListComp #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/15>

-}
module Main (main) where

import Advent (format)
import Advent.Coord (Coord(..), charToVec, coordLines, coordRow, east, west)
import Advent.Search (dfs)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

-- | >>> :main
-- 1499739
-- 1522215
main :: IO ()
main =
 do (input1, input2) <- [format|2024 15 (%s%n)*%n(%s%n)*|]
    let grid = Map.fromList (coordLines input1)
    let start:_ = [p | (p, '@') <- Map.assocs grid]
    let dirs = mapMaybe charToVec (concat input2)
    print (score (fst (foldl sim (grid, start) dirs)))

    let grid2 = Map.fromList (coordLines (map (concatMap expandCell) input1))
    let start:_ = [p  | (p, '@') <- Map.assocs grid2]
    print (score (fst (foldl sim (grid2, start) dirs)))

expandCell :: Char -> String
expandCell '\n' = "\n"
expandCell '#' = "##"
expandCell 'O' = "[]"
expandCell '.' = ".."
expandCell '@' = "@."
expandCell _   = error "bad input"

score :: Map Coord Char -> Int
score m = sum [100 * y + x  | (C y x, c) <- Map.assocs m, c == 'O' || c == '[']

sim :: (Map Coord Char, Coord) -> Coord -> (Map Coord Char, Coord)
sim (grid, start) d
  | '#' `elem` map snd moving = (grid, start)
  | otherwise                 = (grid', start + d)
  where
    moving = filter (\x -> snd x /= '.') (dfs moveStep (start,'@'))
    
    grid' = Map.union (Map.fromList [(p + d, c  ) | (p, c) <- moving])
          $ Map.union (Map.fromList [(p    , '.') | (p, _) <- moving]) grid
    
    vertical = coordRow d /= 0

    moveStep (x, _) =
      map (\p -> (p, grid Map.! p))
      case grid Map.! x of
        '[' -> [x + east | vertical] ++ [x + d]
        ']' -> [x + west | vertical] ++ [x + d]
        'O' ->                          [x + d]
        '@' ->                          [x + d]
        _   -> []
