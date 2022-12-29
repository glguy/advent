{-# Language QuasiQuotes, OverloadedRecordDot, ImportQualifiedPost, BangPatterns #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/19>

This solution uses a few optimizations to achieve lightning fast performance:

* Prune out any state that has the same number of bots at a given time with fewer resources
* Prune out any state that in the best case produces fewer geodes than the any state doing nothing
* Generate a new state for each purchase, not individual timesteps.
* Any bot bought is bought at the earliest possible time.

>>> :{
:main +
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n\
    \Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.\n"
:}
33
3472

-}
module Main where

import Control.Parallel.Strategies (parMap, rseq)
import Control.Parallel (par)
import Data.List (foldl')
import Data.Map qualified as Map

import Advent (format)

type Blueprint = (Int, Int, Int, Int, Int, Int, Int)

-- |
-- >>> :main
-- 1306
-- 37604
main :: IO ()
main = do
    input <- [format|2022 19
        (Blueprint %u: Each ore robot costs %u ore.
         Each clay robot costs %u ore.
         Each obsidian robot costs %u ore and %u clay.
         Each geode robot costs %u ore and %u obsidian.%n)*|]
    let xs = parMap rseq (\b@(i,_,_,_,_,_,_) -> i * solve 24 b) input
    let ys = parMap rseq (solve 32) (take 3 input)
    print (ys `par` sum xs)
    print (product ys)

solve :: Int -> Blueprint -> Int
solve t0 blue = go (Map.singleton t0 [def])
  where
    go q =
      case Map.maxViewWithKey q of
        Nothing -> 0
        Just ((0,sts), _) -> maximum (map (geo . amts) sts)
        Just ((t,sts), q') ->
          go $ Map.unionWith (++) q' $
          Map.fromListWith (++)
          [ (t',[v'])
          | (k,vs) <-
                Map.assocs $
                Map.fromListWith (++)
                [ (bots st, [amts st])
                | let u = maximum (map (underapprox t) sts)
                , st <- sts
                , overapprox t st >= u]
          , v       <- keepBest vs
          , (t',v') <- step blue t k v]

-- | amount of geodes we'd end with if we bought a geode bot every single timestep
overapprox :: Int -> State -> Int
overapprox t st = underapprox t st + t * (t - 1) `div` 2

-- | amount of geodes we'd end with if we didn't buy any more geode bots
underapprox :: Int -> State -> Int
underapprox t st = t * st.bots.geo + st.amts.geo

-- | Remove all resource sets from the list that are dominated by another
-- entry in the list.
keepBest :: [Res] -> [Res]
keepBest = foldl' f []
  where
    f acc x
      | any (\a -> cover a x) acc = acc
      | otherwise = x : filter (not . cover x) acc

-- | Relation for the first element dominating the second.
cover :: Res -> Res -> Bool
cover a b =
    a.ore >= b.ore &&
    a.cla >= b.cla &&
    a.obs >= b.obs &&
    a.geo >= b.geo

data State = State {
    bots :: !Res,
    amts :: !Res
} deriving (Show, Eq, Ord)

data Res = Res {
    ore, cla, obs, geo :: !Int
} deriving (Show, Eq, Ord)

def :: State
def = State {
    bots = Res { ore=1, cla=0, obs=0, geo=0 },
    amts = Res { ore=0, cla=0, obs=0, geo=0 }
}

divUp :: Int -> Int -> Int
divUp x y = (x+y-1) `div` y

step :: Blueprint -> Int -> Res -> Res -> [(Int,State)]
step (_, oreCostOre, claCostOre, obsCostOre, obsCostCla, geoCostOre, geoCostObs) t !bs !as
  | null buys = [(0, State{ bots = bs, amts = as { geo = as.geo + t * bs.geo }}) | t > 0]
  | otherwise = buys
    where
        oreCostMax = oreCostOre `max` claCostOre `max` obsCostOre `max` geoCostOre

        cap b a = State { bots = b , amts = a{
            ore = if b.ore == oreCostMax then min b.ore a.ore else a.ore,
            cla = if b.cla == obsCostCla then min b.cla a.cla else a.cla,
            obs = if b.obs == geoCostObs then min b.obs a.obs else a.obs}}

        buys =
            [(t', cap
                  bs{ geo = bs.geo + 1 }
                  as{ ore = as.ore + bs.ore * dt - geoCostOre
                    , cla = as.cla + bs.cla * dt
                    , obs = as.obs + bs.obs * dt - geoCostObs
                    , geo = as.geo + bs.geo * dt })
                | bs.obs > 0
                , let dt = 1 +
                        max 0 (max
                        ((geoCostOre - as.ore) `divUp` bs.ore)
                        ((geoCostObs - as.obs) `divUp` bs.obs))
                , let t' = t - dt
                , t' >= 0] ++
            [(t', cap
                  bs{ obs = bs.obs + 1 }
                  as{ ore = as.ore + bs.ore * dt - obsCostOre
                    , cla = as.cla + bs.cla * dt - obsCostCla
                    , obs = as.obs + bs.obs * dt
                    , geo = as.geo + bs.geo * dt})
                | bs.obs < geoCostObs
                , bs.cla > 0
                , let dt = 1 + max 0 (max
                        ((obsCostOre - as.ore) `divUp` bs.ore)
                        ((obsCostCla - as.cla) `divUp` bs.cla))
                , let t' = t - dt
                , t' >= 0] ++
            [(t', cap
                  bs{ cla = bs.cla + 1 }
                  as{ ore = as.ore + bs.ore * dt - claCostOre
                    , cla = as.cla + bs.cla * dt
                    , obs = as.obs + bs.obs * dt
                    , geo = as.geo + bs.geo * dt})
                | bs.cla < obsCostCla
                , let dt = 1 + max 0
                        ((claCostOre - as.ore) `divUp` bs.ore)
                , let t' = t - dt
                , t' >= 0] ++
            [(t', cap
                  bs{ ore = bs.ore + 1 }
                  as{ ore = as.ore + bs.ore * dt - oreCostOre
                    , cla = as.cla + bs.cla * dt
                    , obs = as.obs + bs.obs * dt
                    , geo = as.geo + bs.geo * dt})
                | bs.ore < oreCostMax
                , let dt = 1 + max 0
                        ((oreCostOre - as.ore) `divUp` bs.ore)
                , let t' = t - dt
                , t' >= 0]
