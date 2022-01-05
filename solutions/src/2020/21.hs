{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/21>

-}
module Main (main) where

import Advent (countBy, uniqueAssignment)
import Advent.Format (format)
import Data.List (intercalate, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- |
-- >>> :main
-- 2517
-- rhvbn,mmcpg,kjf,fvk,lbmt,jgtb,hcbdb,zrb
main :: IO ()
main =
  do inp <- [format|2020 21 (%s&  %(contains %s&(, )%)%n)*|]
     let [soln]   = uniqueAssignment (toConstraints inp)
         badFoods = Set.fromList (Map.elems soln)

     print (countBy (`Set.notMember` badFoods) (concatMap fst inp))
     putStrLn (intercalate "," (Map.elems soln))

toConstraints :: (Ord a, Ord b) => [([a],[b])] -> Map b (Set a)
toConstraints inp =
  Map.fromListWith Set.intersection
    [(y, Set.fromList xs) | (xs, ys) <- inp, y <- ys]
