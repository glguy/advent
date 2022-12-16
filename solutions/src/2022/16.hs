{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/16>

-}
module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List (tails)

import Advent ( format )

main :: IO ()
main = do
    input <- [format|2022 16 (Valve %s has flow rate=%u; tunnel(|s) lead(|s) to valve(|s) %s&(, )%n)*|]
    let m = Map.fromList [(k, (n, vs)) | (k,n,vs) <- input]

    let routeValues1 = Map.fromListWith max [(open, n) | (_, open, n) <- solver m 30]
    print (maximum routeValues1)
    
    let routeValues2 = Map.fromListWith max [(open, n) | (_, open, n) <- solver m 26]
    print (maximum [ v+v1  | (k,v):ys <- tails (Map.assocs routeValues2), (k1,v1) <- ys, Set.null (Set.intersection k k1)])

solver :: Map String (Int, [String]) -> Int -> [(String, Set String, Int)]
solver m = go [("AA", Set.empty, 0)]
  where
    go x 0 = x
    go x t = go (simplify (concatMap step x)) (t-1)
        where
            step (here, open, n) =
                [(next, open, n) | next <- snd (m Map.! here) ] ++
                [(here, Set.insert here open, n + (t-1) * amt)
                    | Set.notMember here open
                    , let amt = (fst $ m Map.! here), amt /= 0 ]
    simplify xs =
        [(here,open,n) | ((here,open),n) <- Map.assocs (Map.fromListWith max [((here, open), n) | (here, open, n) <- xs])]
