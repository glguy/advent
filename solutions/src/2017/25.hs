{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost, DeriveFunctor #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<http://adventofcode.com/2017/day/25>

Implement a Turing Machine.

-}
module Main where

import Advent.Format ( format )
import Advent.Fix (Fix(Fix), anaFromMap)
import Control.Applicative (many, some, (<|>))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map qualified as Map

data D = Dleft | Dright
  deriving Show

mempty

-- | Print the solution to the task. Input file can be overridden via
-- command-line arguments.
main :: IO ()
main =
 do (start, iter, rules) <- [format|2017 25
      Begin in state %c.%n
      Perform a diagnostic checksum after %d steps.%n
      (%n
      In state %c:%n
        If the current value is 0:%n
          - Write the value %d.%n
          - Move one slot to the @D.%n
          - Continue with state %c.%n
        If the current value is 1:%n
          - Write the value %d.%n
          - Move one slot to the @D.%n
          - Continue with state %c.%n)*|]

    let dirInt Dleft = -1
        dirInt Dright = 1
        writeBool x = 0 /= x
        toRule (s, w0, d0, s0, w1, d1, s1) =
          (s, Rule (Action (writeBool w0) (dirInt d0) s0) (Action (writeBool w1) (dirInt d1) s1))

    let program          = buildProgram (map toRule rules) start
        machine          = Machine mempty 0 program
        Machine tape _ _ = steps iter machine
        checksum         = IntSet.size tape

    print checksum

-- | Step a machine multiple iterations.
steps :: Int {- ^ iterations -} -> Machine -> Machine
steps 0 m = m
steps n m = steps (n-1) $! step m

-- | Advance the tape machine a single step.
step :: Machine -> Machine
step (Machine tape cursor (Fix (Rule a0 a1))) =
  let Action v d p = if IntSet.member cursor tape then a1 else a0
  in Machine (updateSet v cursor tape) (cursor + d) p

-- | When the argument is 'True', insert the given number into the set,
-- otherwise remove it from the set.
updateSet :: Bool -> Int -> IntSet -> IntSet
updateSet True  = IntSet.insert
updateSet False = IntSet.delete

-- | The state of a machine: tape, cursor address, current program
data Machine = Machine !IntSet !Int !(Fix Rule)

-- | Transform a list of named rules into a single program.
buildProgram :: [(Char, Rule Char)] -> Char -> Fix Rule
buildProgram = anaFromMap . Map.fromList

-- | A rule defines a single state. The first action is used when the
-- current value of the tape is 0, The second action is used when the
-- current value of the tape is 1. Actions are parameterized by the
-- type of program to jump to.
data Rule a = Rule (Action a) (Action a) deriving (Show, Functor)

-- | An update action for a rule containing: the new tape value, an
-- offset to the cursor, and the next program state. Actions are
-- parameterized by the type of program to jump to.
data Action a = Action !Bool !Int a deriving (Show, Functor)
