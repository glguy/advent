{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/7>

To compute our thrust controller feedback loop I evaluate the
given Intcode program as a function from input values to output
values. Connecting the outputs of one instance of the program
to the inputs of the next is as simple as composing the two
functions together. Thanks to non-strict evaluation I can
pass the output of a composition of these functions back in
as its own input!

-}
module Main (main) where

import Advent (format)
import Data.Function (fix)
import Data.List (permutations)
import Intcode (intcodeToList)

-- | A function from a list of input values to a list of output values.
type ListFn = [Int] -> [Int]

-- | >>> :main
-- 34852
-- 44282086
main :: IO ()
main =
  do pgm <- intcodeToList <$> [format|2019 7 %d&,%n|]
     print (part1 pgm)
     print (part2 pgm)

-- | Run the given amplitude controller in a feedback loop across
-- all permutations of the settings 0 through 4. Returns the
-- maximum initial thruster output.
--
-- >>> part1 (intcodeToList [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
-- 43210
-- >>> part1 (intcodeToList [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0])
-- 54321
-- >>> part1 (intcodeToList [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])
-- 65210
part1 ::
  ListFn {- ^ amplifier controller software   -} ->
  Int    {- ^ maximum initial thruster output -}
part1 = optimize head [0..4]

-- | Run the given amplitude controller in a feedback loop across
-- all permutations of the settings 5 through 9. Returns the
-- maximum final thruster output.
--
-- >>> part2 (intcodeToList [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
-- 139629729
-- >>> :{
-- >>> part2 (intcodeToList [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-- >>>                       -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
-- >>>                       53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])
-- >>> :}
-- 18216
part2 ::
  ListFn {- ^ amplifier controller software -} ->
  Int    {- ^ maximum final thruster output -}
part2 = optimize last [5..9]


optimize ::
  Ord a =>
  ([Int] -> a) {- ^ output characterization    -} ->
  [Int]        {- ^ phase available            -} ->
  ListFn       {- ^ phases to outputs          -} ->
  a            {- ^ maximized characterization -}
optimize f phases pgm = maximum [f (thrustController pgm p) | p <- permutations phases]

-- | Given a amplifier controller software function and a list of
-- phase settings, generate the resulting list of thruster outputs.
--
-- Once instances of the control software is started for each phase setting,
-- the instances are all sequenced together into a single loop. A starting
-- @0@ element is added as an initial input.
--
-- >>> thrustController (intcodeToList [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) [4,3,2,1,0]
-- [43210]
-- >>> thrustController (intcodeToList [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]) [9,8,7,6,5]
-- [129,4257,136353,4363425,139629729]
thrustController ::
  ListFn {- ^ amplifier controller software -} ->
  ListFn {- ^ thrust controller             -}
thrustController ctrl phases = tieknot [ctrl << p | p <- phases]

-- | Create a feedback loop given the initialized controllers
-- and return the thruster outputs. Feed an initial @0@ value
-- into the loop.
--
-- >>> tieknot [map (2*), map (1+), take 5]
-- [1,3,7,15,31]
tieknot ::
  [ListFn] {- ^ initialized amplifier controllers -} ->
  [Int]    {- ^ thruster outputs                  -}
tieknot fs = fix (composeLR fs << 0)

-- | Compose list functions from left-to-right. Inputs go into
-- first function and outputs come from last function.
--
-- >>> composeLR [(2*),(1+)] 3
-- 7
composeLR :: [a -> a] -> (a -> a)
composeLR = foldl (flip (.)) id

-- | Feed a single input into a list function.
--
-- >>> (map (*2) << 10) [5,6,7]
-- [20,10,12,14]
(<<) :: ListFn -> Int -> ListFn
(f << x) xs = f (x:xs)
