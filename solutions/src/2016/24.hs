{-# Language ImportQualifiedPost, BangPatterns #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/24>

-}
module Main where

import Advent (getInputArray)
import Advent.Coord (cardinal, Coord)
import Advent.Search (bfsOn)
import Advent.SmallSet (SmallSet)
import Advent.SmallSet qualified as SBS
import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as Array
import Data.Char (digitToInt, isDigit)
import Data.Maybe (mapMaybe)

data Entry = Entry {-# UNPACK #-} !Coord !SmallSet
  deriving (Eq, Ord)

-- | >>> :main
-- 498
-- 804
main :: IO ()
main =
  do maze <- getInputArray 2016 24

     let targets = SBS.fromList
                 $ mapMaybe digitToInt'
                 $ Array.elems maze

         [start] = [ c | (c,x) <- Array.assocs maze, x == '0' ]

         endings =
           [ (here,steps)
              | (seen,here,steps) <-
                    bfsOn
                      (\(seen,here,_steps) -> Entry here seen)
                      (next maze)
                      (SBS.singleton 0, start,0)
              , seen == targets ]

     print $ head [ steps | (_  ,steps) <- endings ]
     print $ head [ steps | (end,steps) <- endings, end == start ]

next ::
  UArray Coord Char ->
  (SmallSet, Coord, Int) ->
  [(SmallSet, Coord, Int)]
next maze (seen,here,steps) =
  [ (seen',here',steps')
    | let !steps' = steps + 1
    , here' <- cardinal here
    , let x = maze Array.! here'
    , x /= '#'
    , let !seen' = case digitToInt' x of
                     Just i  -> SBS.insert i seen
                     Nothing -> seen
    ]

digitToInt' :: Char -> Maybe Int
digitToInt' x
  | isDigit x = Just (digitToInt x)
  | otherwise = Nothing
