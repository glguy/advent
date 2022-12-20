{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/19>

This solution is quite rought and I intend to clean it up once I've had more time to think about it.

-}
module Main where

import Data.Set qualified as Set

import Advent (format, ordNub)
import Control.Parallel.Strategies ( parMap, rseq )

type Blueprint = (Int, Int, Int, Int, Int, Int, Int)

-- |
-- >>> :main
-- 1306
-- 37604
main :: IO ()
main = do
    input <- [format|2022 19 (Blueprint %u: Each ore robot costs %u ore. Each clay robot costs %u ore. Each obsidian robot costs %u ore and %u clay. Each geode robot costs %u ore and %u obsidian.%n)*|]
    let xs = parMap rseq (\b@(i,_,_,_,_,_,_) -> i*solve 24 b def) input
    print (sum xs)
    let ys = parMap rseq (\b -> solve 32 b def) (take 3 input)
    print (product ys)

solve :: Int -> Blueprint -> State -> Int
solve limit blue st0 = go 0 (Set.singleton (0,0,0,0)) [st0]
  where
    go t _ sts | t == limit = maximum (map geo sts)
    go t seen sts = go (t+1) (Set.union seen (Set.fromList (map rep sts'))) sts'
        where
            sts' =
                map earn sts ++
                filter (\x -> rep x `Set.notMember` seen) (ordNub (concatMap (actions blue) sts))

-- | Characterize the state by number of bots purchased
rep :: State -> (Int,Int,Int,Int)
rep st = (oreBots st, clayBots st, obsBots st, geodeBots st)

data State = State {
    oreBots, clayBots, obsBots, geodeBots, ore, clay, obs, geo :: !Int
} deriving (Show, Eq, Ord)

def :: State
def = State {
    oreBots=1, clayBots=0, obsBots=0, geodeBots=0, ore=0, clay=0, obs=0, geo=0
}

actions :: Blueprint -> State -> [State]
actions (_, oreCostOre, clayCostOre, obsCostOre, obsCostClay, geoCostOre, geoCostObs) st =
    [st' { ore = ore st' - geoCostOre, obs = obs st' - geoCostObs, geodeBots = geodeBots st' + 1 }
        | geoCostOre <= ore st, geoCostObs <= obs st -- affordable
        ] <++
    [st' { ore = ore st' - obsCostOre, clay = clay st' - obsCostClay, obsBots = obsBots st' + 1 }
        | obsCostOre <= ore st, obsCostClay <= clay st -- affordable
        , obsBots st < geoCostObs                      -- useful
        ] <++
    [st' { ore = ore st' - clayCostOre, clayBots = clayBots st' + 1 }
        | clayCostOre <= ore st                        -- affordable
        , clayBots st < obsCostClay                    -- useful
        ] ++
    [st' { ore = ore st' - oreCostOre, oreBots = oreBots st' + 1 }
        | oreCostOre <= ore st                         -- affordable
        , oreBots st < oreCostMax                      -- useful
        ]
    where
        st' = earn st
        oreCostMax = (oreCostOre `max` clayCostOre `max` obsCostOre `max` geoCostOre)

earn :: State -> State
earn st =
    st { ore  = ore  st + oreBots   st,
         clay = clay st + clayBots  st,
         obs  = obs  st + obsBots   st,
         geo  = geo  st + geodeBots st }

-- I tried with this optimization and without it, and it runs faster preferring to buy ore and obs bots when possible
-- but I doubt that it's always a good idea in general
(<++) :: [a] -> [a] -> [a]
[] <++ xs = xs
xs <++ _ = xs

infixr 5 <++