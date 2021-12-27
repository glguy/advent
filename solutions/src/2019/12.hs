{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/12>

The stepping function is invertible, so any cycles must include
the starting point. This means to find a cycle we just search
for the starting point rather than remembering all states along
the way.

-}
module Main (main) where

import Advent (format)
import Data.List (transpose, elemIndex, foldl')
import Data.Char (isAlpha)

-- | One-dimensional particle with a position and velocity.
data Particle = P !Int !Int -- ^ position velocity
  deriving (Eq, Ord, Show)

-- | >>> :main
-- 8538
-- 506359021038056
main :: IO ()
main =
 do let toPs (x,y,z) = [P x 0, P y 0, P z 0]
    threeD_sim <- map toPs <$> [format|12 (<x=%d, y=%d, z=%d>%n)*|]
    let oneD_sims = transpose threeD_sim
    print (part1 oneD_sims)
    print (part2 oneD_sims)


part1 :: [[Particle]] -> Int
part1 oneD_sims = sum (map energy threeD_sim1000)
  where
    oneD_sims1000  = [ iterate stepParticles sim !! 1000 | sim <- oneD_sims ]
    threeD_sim1000 = transpose oneD_sims1000


part2 :: [[Particle]] -> Int
part2 oneD_sims = foldl' lcm 1 periods
  where
    periods = map (repeatLength . iterate stepParticles) oneD_sims


-- | Compute the energy of a multi-dimensional particle given
-- its dimensional components.
energy :: [Particle] -> Int
energy ps = sum [ abs x | P x _ <- ps ] * sum [ abs v | P _ v <- ps ]

repeatLength :: Eq a => [a] -> Int
repeatLength [] = error "repeatList: no cycle"
repeatLength (x:xs) = 1 + n
  where Just n = elemIndex x xs

-- | Advance a particle by its current velocity.
move :: Particle -> Particle
move (P x dx) = P (x+dx) dx

-- | Single step of a one-dimensional, n-body system.
stepParticles :: [Particle] -> [Particle]
stepParticles ps = [ move (foldl' gravity p ps) | p <- ps ]

-- | Apply gravity to the first particle based on the second.
gravity :: Particle -> Particle -> Particle
gravity (P x v) (P y _) = P x (v + signum (y-x))
