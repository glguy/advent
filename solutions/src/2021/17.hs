{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/17>

Launch probes to hit a target.

This solution relies on the target being in quadrant IV.
You could do the math for the other quadrants, but I didn't
bother!

Boundary conditions:

* x velocity must be non-negative to hit things
  in quadrant IV
* x velocity must not be more than @xhi@ or the
  probe will miss on the first timestep.
* y velocity can't be more than @ylo@ or it will
  miss on the first time step.
* y velocity can't be more than @-ylo@ or it will
  skip right past the target on its way back down.

-}
module Main (main) where

import Advent.Format (format)
import Control.Monad (when)

-- | The state of a traveling probe
data Probe = P !Int !Int !Int !Int -- ^ x-position y-position x-velocity y-velocity
  deriving Show

-- | Advance the probe one timestep
step :: Probe -> Probe
step (P x y vx vy) = P (x+vx) (y+vy) (vx-signum vx) (vy-1)

-- | >>> :main
-- 10585
-- 5247
main :: IO ()
main =
  do (xlo,xhi,ylo,yhi) <- [format|2021 17 target area: x=%d..%d, y=%d..%d%n|]
     when (xlo < 0 || yhi > 0) (fail "I didn't do enough math for this case")

     let ys = [y | vx <- [0 .. xhi], vy <- [ylo .. -ylo], Just y <- [sim xlo xhi ylo yhi vx vy]]
     print (maximum ys)
     print (length  ys)

-- | Run a simulation returning the maximum height seen if
-- the probe ever succeeds in hitting the target.
--
-- >>> sim 20 30 (-10) (-5) 6 9
-- Just 45
sim ::
  Int {- ^ target x lo -} ->
  Int {- ^ target x hi -} ->
  Int {- ^ target y lo -} ->
  Int {- ^ target y hi -} ->
  Int {- ^ initial x velocity -} ->
  Int {- ^ initial y velocity -} ->
  Maybe Int {- ^ maximum height if successful -}
sim xlo xhi ylo yhi vx0 vy0 = go 0 (P 0 0 vx0 vy0)
  where
    go best p@(P x y _ _)
      | y < ylo || x > xhi                     = Nothing -- too far
      | xlo <= x, x <= xhi, ylo <= y, y <= yhi = Just best
      | otherwise                              = go (max y best) (step p)
