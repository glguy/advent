{-# Language QuasiQuotes, ImportQualifiedPost, BlockArguments #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/16>

>>> :{
:main +
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
    \Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
    \Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
    \Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
    \Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
    \Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
    \Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
    \Valve HH has flow rate=22; tunnel leads to valve GG\n\
    \Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
    \Valve JJ has flow rate=21; tunnel leads to valve II\n"
:}
1651
1707

-}
module Main where

import Data.List (tails, foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as SMap
import Data.Maybe (maybeToList)

import Advent (format)
import Advent.SmallSet (SmallSet)
import Advent.SmallSet qualified as SmallSet

-- |
-- >>> :main
-- 1820
-- 2602
main :: IO ()
main = do
    input <- [format|2022 16 (Valve %s has flow rate=%u; tunnel(|s) lead(|s) to valve(|s) %s&(, )%n)*|]

    let distances1 = Map.fromList [((k,v),1) | (k, _, vs) <- input, v <- vs]
    let distances  = fw [k | (k,_,_) <- input] distances1
    let flows      = Map.fromList [(k, n) | (k, n, _) <- input, n > 0]
    let graph      = renumber $ 
                     Map.fromListWith (++)
                        [(src, [(dst,cost+1,flow)])
                            | ((src,dst),cost) <- Map.assocs distances
                            , src == "AA" || Map.member src flows, src /= dst
                            , flow <- maybeToList (Map.lookup dst flows)]

    let routeValues1 = solve graph 30
    print (maximum routeValues1)

    let routeValues2 = solve graph 26
    print (maximum [v1+v2 | (open1,v1) : elephants <- tails (Map.assocs routeValues2),
                            (open2,v2) <- elephants,
                            SmallSet.disjoint open1 open2])

solve :: Map Int [(Int, Int, Int)] -> Int -> Map SmallSet Int
solve graph time0 =
    SMap.fromListWith max (go [(time0, 0, SmallSet.empty, 0)])
    where
        go xs = [(open,flow) | (_,_,open,flow) <- xs] ++ concatMap (go . step) xs
        step (t, here, open, flow) =
            [ (t', next, SmallSet.insert next open, flow + t' * valve)
                | (next, cost, valve) <- graph Map.! here
                , not (SmallSet.member next open)
                , let t' = t - cost
                , t' > 0
            ]

-- | Replace all the string names with sequentially assign Int names to speed
-- up comparisons and enable the use of SmallSet
renumber :: Map String [(String, Int, Int)] -> Map Int [(Int, Int, Int)]
renumber graph =
    Map.fromList [ (a Map.! k, [(a Map.! x,y,z) | (x,y,z) <- vs])
                 | (k, vs) <- Map.toList graph]
    where
        a = Map.fromList (zip (Map.keys graph) [0..])

-- | Floyd-Warshall shortest paths
fw ::
    Ord k =>
    [k]           {- ^ all verticies -} ->
    Map (k,k) Int {- ^ distances between a pair of verticies -} ->
    Map (k,k) Int {- ^ shortest distance between two verticies -}
fw keys = each \k -> each \i -> each \j dists ->
    case (Map.lookup (i,k) dists, Map.lookup (k,j) dists) of
        (Just d1, Just d2) -> SMap.insertWith min (i,j) (d1+d2) dists
        _                  -> dists
    where
        each g z = foldl' (flip g) z keys
