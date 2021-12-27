{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<http://adventofcode.com/2017/day/20>

Day 20 has us implement a simple particle motion simulator.

Instead of implementing some detection for a stable state
I just run this program and wait a few moments for things
to stabilize before I kill it. I print incremental output so I
can see how quickly things seem to settle.

-}
module Main where

import Advent (format, minimumMaybe)
import Data.Foldable (toList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (foldl', tails, intersect)
import Data.Map qualified as Map
import Linear (V2(V2), V3(V3), quadrance)
import Linear.Matrix as LM (transpose)

-- $setup
-- >>> import Text.Megaparsec (parseMaybe)

-- | Print the solutions. Input file can be overridden via command-line
-- arguments.
main :: IO ()
main =
 do input <- [format|20 (p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>%n)*|]
    let particles = [fmap fromIntegral <$> V3 (V3 p1 p2 p3) (V3 v1 v2 v3) (V3 a1 a2 a3) | (p1,p2,p3,v1,v2,v3,a1,a2,a3) <- input]
    print (part1 particles)
    print (part2 particles)

-- | Characterize a particle by list of derivatives. The first vector is
-- is the position of the particle. Each of the following vectors is
-- an increasingly higher order derivative of the position.
type Particle = V3 (V3 Double)

-- | Compute the infinite list of indexes of the particles that are
-- nearest to the origin while iterating the system one time step
-- at a time.
part1 :: [Particle] -> Int
part1 = minimumIndexOn (fmap quadrance)

part2 :: [Particle] -> Int
part2 ps = length ps - IntSet.size (process (toEvents ps))

stepParticle :: Particle -> Particle
stepParticle (V3 a v s) = V3 a v' s'
  where
    v' = a + v
    s' = s + v'

-- | Compute the index of the list element with the minimum projection.
--
-- >>> minimumIndexOn negate [3, -10, 5, -9]
-- 2
minimumIndexOn :: Ord b => (a -> b) {- ^ projection -} -> [a] -> Int
minimumIndexOn f xs = snd (minimum (map f xs `zip` [0..]))


collide :: V3 (V3 Double) -> V3 (V3 Double) -> Maybe Double
collide p1 p2
  = minimumMaybe     -- only the earliest collision will matter
  $ filter (>= 0)    -- simulation starts and time 0 and moves forward
  $ foldl1 intersect -- x,y,z components must collide at same time step
  $ fmap (zeros . toPoly)
  $ p1 - p2

-- | Compute coefficients of the polynomial corresponding
-- to a triple of the particle's acceleration, velocity, and position.
toPoly :: V3 Double -> V3 Double
toPoly (V3 a v s) = V3 (a/2) (a/2 + v) s

-- | Given coefficients @'V3' a b c@ compute the values of @x@
-- such that @a*x^2 + b*x + c = 0@. When all coefficients are
-- @0@ we just return @0@ as this is enough for our purposes.
zeros ::
  V3 Double {- ^ polynomial coefficients                       -} ->
  [Double]  {- ^ list of values when polynomial evaluates to 0 -}
zeros (V3 0 0 0) = [0] -- always zero, time 0 is the earliest we care
zeros (V3 0 0 _) = []
zeros (V3 0 b c) = [-c/b]
zeros (V3 a b c) =
  case compare z 0 of
    LT -> []
    EQ -> [-b / 2*a]
    GT -> [(-b + sqrt z) / (2*a), (-b - sqrt z) / (2*a)]
  where
    z = b*b - 4*a*c

-- | Compute the collisions that will happen between a list of
-- particles grouped by the time-step that they happen at.
toEvents :: [Particle] -> [[V2 Int]]
toEvents ps
  = toList
  $ Map.fromListWith (++)
    [ (t,[V2 i j])
      | (i,v1):vs <- tails ([0..] `zip` map LM.transpose ps)
      , (j,v2)    <- vs
      , Just t    <- [collide v1 v2] ]

process :: [[V2 Int]] -> IntSet
process = foldl' runGeneration IntSet.empty

runGeneration ::
  IntSet   {- ^ previously collided particles       -} ->
  [V2 Int] {- ^ possible collisions this time-step  -} ->
  IntSet   {- ^ dead particles after this time-step -}
runGeneration dead xs =
  foldl' (flip IntSet.insert) dead
    [ c | x <- xs
        , all (`IntSet.notMember` dead) x
        , c <- toList x ]
