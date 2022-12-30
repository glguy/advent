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
import Data.Map qualified as Map
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (maybeToList)

import Advent (format)
import Advent.SmallSet (SmallSet)
import Advent.SmallSet qualified as SmallSet

-- |
-- >>> :main
-- 1820
-- 2602
main :: IO ()
main =
 do (aa, input) <-
      renumber <$>
      [format|2022 16
        (Valve %s has flow rate=%u;
         tunnel(|s) lead(|s) to valve(|s) %s&(, )%n)*|]

    let distances1 = IntMap.fromList [(k, IntMap.fromList [(v,1) | v <- vs]) | (k, _, vs) <- input]
    let distances  = fw (IntMap.keys distances1) distances1
    let flows      = IntMap.fromList [(k, n) | (k, n, _) <- input, n > 0]
    let graph      = buildEdges aa $
                     IntMap.fromListWith (++)
                        [(src, [(dst,cost+1,flow)])
                            | (src,m) <- IntMap.assocs distances
                            , src == aa || IntMap.member src flows
                            , (dst,cost) <- IntMap.assocs m
                            , src /= dst
                            , flow <- maybeToList (IntMap.lookup dst flows)]

    let routeValues1 = solve graph 30
    print (maximum routeValues1)

    let routeValues2 = solve graph 26
    print (maximum [v1+v2
        | (open1,v1) : elephants <- tails (IntMap.assocs routeValues2)
        , (open2,v2) <- elephants
        , SmallSet.disjoint (SmallSet.SmallSet (fromIntegral open1))
                            (SmallSet.SmallSet (fromIntegral open2))])

-- | Find the maximum water flow achievable from activating all possible combinations
-- of valves.
solve ::
    Edges      {- graph: source to (dest, distance, flow) -} ->
    Int        {- starting time -} ->
    IntMap Int {- map of opened valves to maximum flow -}
solve start time0 = IntMap.fromListWith max (go [S time0 start SmallSet.empty 0])
    where
        go xs = [(fromIntegral (SmallSet.setRep open),flow) | S _ _ open flow <- xs] ++ concatMap (go . step) xs
        step (S t (Node graph) open flow) =
            [S t' graph' (SmallSet.union next open) (flow + t' * valve)
                | (graph', next, cost, valve) <- graph
                , SmallSet.disjoint next open
                , let t' = t - cost
                , t' > 0]

data S = S !Int Edges !SmallSet !Int

newtype Edges = Node [(Edges, SmallSet, Int, Int)]

renumber :: [(String, Int, [String])] -> (Int, [(Int, Int, [Int])])
renumber xs = (f "AA", [(f k, x, map f ks) | (k,x,ks) <- xs])
  where
    ns = Map.fromList (zip [k | (k,_,_) <- xs] [0..])
    f i = ns Map.! i

-- | Replace all the string names with sequentially assigned Int names to
-- speed up comparisons and enable the use of SmallSet
buildEdges :: Int -> IntMap [(Int, Int, Int)] -> Edges
buildEdges aa graph = m IntMap.! aa
    where
        m = fmap (Node . map f) graph
        f (n,x,y) = (m IntMap.! n, SmallSet.singleton n, x, y)

-- | Floyd-Warshall shortest paths
fw ::
    [Int]               {- ^ all vertices -} ->
    IntMap (IntMap Int) {- ^ distances between a pair of vertices -} ->
    IntMap (IntMap Int) {- ^ shortest distance between two vertices -}
fw keys = each \k -> each \i -> each \j dists ->
    case (lk i k dists, lk k j dists) of
        (Just d1, Just d2) ->
            IntMap.insertWith (IntMap.unionWith min) i (IntMap.singleton j (d1+d2)) dists
        _ -> dists
    where
        each g z = foldl' (flip g) z keys
        lk i j m =
         do m' <- IntMap.lookup i m
            IntMap.lookup j m'
