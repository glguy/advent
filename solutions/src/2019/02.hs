{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/2>

-}
module Main (main) where

import Advent.Format (format)
import Intcode

-- | >>> :main
-- 7210630
-- 3892
main :: IO ()
main =
  do pgm <- new <$> [format|2019 2 %d&,%n|]
     print (startup 12 2 pgm)
     print (head [ 100 * noun + verb
                 | noun <- [0..99]
                 , verb <- [0..99]
                 , startup noun verb pgm == 19690720 ])

-- | Run the given program after assigning the given noun and verb.
startup :: Int {- ^ noun -} -> Int {- ^ verb -} -> Machine -> Int
startup noun verb
  = (! 0)
  . runPgm
  . set 1 noun
  . set 2 verb

-- | Run the given program starting at the given program counter
-- returning the initial memory value once the program halts.
--
-- >>> let check = memoryList . runPgm . new
-- >>> check [1,0,0,0,99]
-- [2,0,0,0,99]
-- >>> check [2,3,0,3,99]
-- [2,3,0,6,99]
-- >>> check [2,4,4,5,99,0]
-- [2,4,4,5,99,9801]
-- >>> check [1,1,1,4,99,5,6,0,99]
-- [30,1,1,4,2,5,6,0,99]
-- >>> check [1,9,10,3,2,3,11,0,99,30,40,50]
-- [3500,9,10,70,2,3,11,0,99,30,40,50]
runPgm :: Machine -> Machine
runPgm mach =
  case step mach of
    Step mach'     -> runPgm mach'
    StepHalt       -> mach
    _              -> error "Unexpected step on day 2"
