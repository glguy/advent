{-# Language QuasiQuotes, ImportQualifiedPost, BangPatterns #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/23>

-}
module Main (main) where

import Advent (format, ordNub)
import Advent.MaxClique (maxCliques)
import Data.Foldable (maximumBy)
import Data.List (intercalate, sort, tails)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 1227
-- cl,df,ft,ir,iy,ny,qp,rb,sh,sl,sw,wm,wy
main :: IO ()
main =
 do input <- [format|2024 23 (%s-%s%n)*|]
    let nodes = ordNub [n | (a,b) <- input, n <- [a,b]]
    let edges = Map.fromListWith Set.union [(min a b, Set.singleton (max a b)) | (a,b) <- input]
    let hasEdge a b = Set.member (max a b) (Map.findWithDefault Set.empty (min a b) edges)
    print $ length
      [ ()
      | (a, bs) <- Map.assocs edges
      , b:cs <- tails (Set.elems bs)
      , c    <- cs
      , hasEdge b c
      , any (\n -> head n == 't') [a,b,c]  
      ]
    putStrLn $ intercalate "," $ sort
             $ maximumBy (comparing length)
             $ maxCliques hasEdge nodes
