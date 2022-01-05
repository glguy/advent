{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/14>

-}
module Main (main) where

import Advent.Format (format)
import Data.List (foldl', sortOn)
import Data.Map (Map)
import Data.Map qualified as Map

data Recipes = Recipes
  { order :: [String]
  , parts :: Map String (Int, [(Int, String)])
  }

type Reaction = ([Component], Int, String)
type Component = (Int, String)

-- | >>> :main
-- 751038
-- 2074843
main :: IO ()
main =
  do inp <- [format|2019 14 ((%u %s)&(, ) => %u %s%n)*|]
     let recipes = mkRecipes inp

     print (oreNeeded recipes 1)

     let p i = oreNeeded recipes i <= 1000000000000
     print (expSearch p 1)

oreNeeded :: Recipes -> Int {- ^ fuel amount -} -> Int {- ^ ore amount -}
oreNeeded recipes n =
  foldl' (react (parts recipes)) (Map.singleton "FUEL" n) (order recipes) Map.! "ORE"

react ::
  Map String (Int, [(Int, String)])    ->
  Map String Int {- ^ items needed  -} ->
  String         {- ^ item to react -} ->
  Map String Int {- ^ items needed  -}
react recipes need item = Map.unionWith (+) need1 need2
  where
    needed = Map.findWithDefault 0 item need

    (makes, needs) = recipes Map.! item
    n = needed `divUp` makes

    need1 = Map.delete item need
    need2 = Map.fromListWith (+) [ (k,n*v) | (v,k) <- needs ]

-- | Integer division that rounds up instead of down.
divUp :: Integral a => a -> a -> a
x `divUp` y = (x + y - 1) `div` y

mkRecipes :: [Reaction] -> Recipes
mkRecipes xs = Recipes
  { order = sortOn depth [n | (_, _, n) <- xs ]
  , parts = partsMap
  }
  where
    partsMap       = Map.fromList [ (dst, (n, src))  | (src, n, dst) <- xs ]
    toDepth (_,ys) = foldl' min 0 [ depth y | (_,y) <- ys ] - 1
    depthMap       = fmap toDepth partsMap
    depth x        = Map.findWithDefault (0::Int) x depthMap

------------------------------------------------------------------------

expSearch ::
  (Int -> Bool) {- ^ predicate        -} ->
  Int           {- ^ small enough     -} ->
  Int           {- ^ largest possible -}
expSearch p lo = go (lo+1)
  where
    go hi
      | p hi      = go (2*hi)
      | otherwise = binSearch p lo hi

binSearch ::
  (Int -> Bool) {- ^ predicate    -} ->
  Int           {- ^ small enough -} ->
  Int           {- ^ too big      -} ->
  Int
binSearch p lo hi
  | lo + 1 == hi = lo
  | p mid        = binSearch p mid hi
  | otherwise    = binSearch p lo mid
  where
    mid = lo + (hi - lo) `div` 2
