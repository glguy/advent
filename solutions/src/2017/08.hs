{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/8>

Day 8 poses a problem of parsing a simple programming language,
executing it, and computing simple metrics on the values stored
in variables during execution.

>>> :{
:main +
  "b inc 5 if a > 1\n\
  \a inc 1 if b < 5\n\
  \c dec -10 if a >= 1\n\
  \c inc -20 if c == 10\n"
:}
1
10

-}
module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

import Advent (format, maximumMaybe, stageTH)

data C = Cinc | Cdec deriving Show

stageTH

-- | Compute solution to Day 8. Input file can be overridden with command-line
-- arguments.
--
-- >>> :main
-- 4163
-- 5347
main :: IO ()
main =
 do input <- [format|2017 8 (%s @C %d if %s %s %d%n)*|]
    let regmaxes = map (fromMaybe 0 . maximumMaybe) (scanl interpret mempty input)
    print (last regmaxes)
    print (maximum regmaxes)

-- | Given registers and a statement, compute the resulting registers.
interpret ::
  Map String Int {- ^ incoming registers -} ->
  (String, C, Int, String, String, Int) {- ^ statement -} ->
  Map String Int {- ^ outgoing registers -}
interpret regs (r1,op1,n1,r2,op2,n2)
  | toCompare op2 (Map.findWithDefault 0 r2 regs) n2 =
      Map.alter (Just . toArith op1 n1 . fromMaybe 0) r1 regs
  | otherwise = regs

-- | Convert the string representation of a comparison to a function.
toCompare ::
  String               {- ^ name     -} ->
  (Int -> Int -> Bool) {- ^ function -}
toCompare "<"  = (< )
toCompare ">"  = (> )
toCompare ">=" = (>=)
toCompare "<=" = (<=)
toCompare "!=" = (/=)
toCompare "==" = (==)
toCompare name = error ("Bad comparison function: " ++ name)

-- | Convert the string representation of an arithmetic operation to a function.
toArith ::
  C                   {- ^ name     -} ->
  (Int -> Int -> Int) {- ^ function -}
toArith Cinc = (+)
toArith Cdec = subtract
