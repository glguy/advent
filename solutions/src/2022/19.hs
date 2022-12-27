{-# Language QuasiQuotes, ImportQualifiedPost, BangPatterns #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/19>

>>> :{
:main +
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n\
    \Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.\n"
:}
33

-}
module Main (main) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.List (foldl', maximumBy)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Advent (format, ordNub)

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
    print (sum xs)
    let ys = parMap rseq (solve 32) (take 3 input)
    print (product ys)

solve :: Int -> Blueprint -> Int
solve t0 blue@(i,_,_,_,_,_,_) = go Set.empty (Map.singleton t0 [def])
  where
    go seen q =
        case Map.maxViewWithKey q of
            Nothing -> 0
            Just ((0,sts), _) -> maximum (map geo sts)
            Just ((t,sts), q') ->
                go (Set.fromList (map botRep sts) <> seen) $
                foldl' (\q_ st -> foldl' ins q_ (step blue t st)) q' sts'
              where
                sts' = filter (\x -> Set.notMember (botRep x) seen) (ordNub sts)

    ins q (t, st) = Map.insertWith (++) t [st] q

botRep :: State -> (Int, Int, Int, Int)
botRep st = (oreBots st, claBots st, obsBots st, geoBots st)

data State = State {
    oreBots, claBots, obsBots, geoBots,
    ore, cla, obs, geo :: !Int
} deriving (Show, Eq, Ord)

def :: State
def = State {
    oreBots=1, claBots=0, obsBots=0, geoBots=0, ore=0, cla=0, obs=0, geo=0
}

divUp :: Int -> Int -> Int
divUp x y = (x+y-1) `div` y

step :: Blueprint -> Int -> State -> [(Int,State)]
step (_, oreCostOre, claCostOre, obsCostOre, obsCostCla, geoCostOre, geoCostObs) t !st
  | null buys = [(0, st { geo = geo st + t * geoBots st }) | t > 0]
  | otherwise = buys
    where
        oreCostMax = oreCostOre `max` claCostOre `max` obsCostOre `max` geoCostOre
        buys =
            [(t', st
                { ore = ore st + oreBots st * dt - geoCostOre
                , cla = cla st + claBots st * dt
                , obs = obs st + obsBots st * dt - geoCostObs
                , geo = geo st + geoBots st * dt
                , geoBots = geoBots st + 1 })
                | obsBots st > 0
                , let dt = 1 +
                        max 0 (max
                        ((geoCostOre - ore st) `divUp` oreBots st)
                        ((geoCostObs - obs st) `divUp` obsBots st))
                , let t' = t - dt
                , t' >= 0] ++
            [(t', st
                { ore = ore st + oreBots st * dt - obsCostOre
                , cla = cla st + claBots st * dt - obsCostCla
                , obs = obs st + obsBots st * dt
                , geo = geo st + geoBots st * dt
                , obsBots = obsBots st + 1 })
                | obsBots st < geoCostObs
                , claBots st > 0
                , let dt = 1 + max 0 (max
                        ((obsCostOre - ore st) `divUp` oreBots st)
                        ((obsCostCla - cla st) `divUp` claBots st))
                , let t' = t - dt
                , t' >= 0] ++
            [(t', st
                { ore = ore st + oreBots st * dt - claCostOre
                , cla = cla st + claBots st * dt
                , obs = obs st + obsBots st * dt
                , geo = geo st + geoBots st * dt
                , claBots = claBots st + 1 })
                | claBots st < obsCostCla
                , let dt = 1 + max 0
                        ((claCostOre - ore st) `divUp` oreBots st)
                , let t' = t - dt
                , t' >= 0] ++
            [(t', st
                { ore = ore st + oreBots st * dt - oreCostOre
                , cla = cla st + claBots st * dt
                , obs = obs st + obsBots st * dt
                , geo = geo st + geoBots st * dt
                , oreBots = oreBots st + 1 })
                | oreBots st < oreCostMax
                , let dt = 1 + max 0
                        ((oreCostOre - ore st) `divUp` oreBots st)
                , let t' = t - dt
                , t' >= 0]
