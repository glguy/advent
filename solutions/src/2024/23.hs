{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/23>

-}
module Main (main) where

import Advent (format)
import Advent.MaxClique (maxCliques)
import Data.Foldable (maximumBy)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set

-- | >>> :main
-- 1227
-- cl,df,ft,ir,iy,ny,qp,rb,sh,sl,sw,wm,wy
main :: IO ()
main =
 do input <- [format|2024 23 (%s-%s%n)*|]
    let ns = Set.elems (Set.fromList [x | (a, b) <- input, x <- [a, b]])
        g = Map.fromListWith (<>) [(min a b, Set.singleton (max a b)) | (a, b) <- input]
        hasEdge a b = maybe False (Set.member (max a b)) (Map.lookup (min a b) g)
    print $ length
      [ ()
      | (a, aEdges) <- Map.assocs g
      , (b, bEdges) <- Map.assocs (Map.restrictKeys g aEdges)
      , c <- Set.elems (Set.intersection aEdges bEdges)
      , any (\n -> head n == 't') [a, b, c]
      ]
    putStrLn $ intercalate ","
             $ maximumBy (comparing length)
             $ maxCliques hasEdge ns
