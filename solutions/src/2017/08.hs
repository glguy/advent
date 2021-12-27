{-# Language RankNTypes, QuasiQuotes, TemplateHaskell #-} -- for the type signature of register
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 8 poses a problem of parsing a simple programming language,
executing it, and computing simple metrics on the values stored
in variables during execution.
-}
module Main where

import Advent       (format)
import Data.Map     (Map)
import Control.Lens (Lens', at, folded, non, over, maximumOf, view)

data C = Cinc | Cdec deriving Show

mempty

-- | Compute solution to Day 8. Input file can be overridden with command-line
-- arguments.
main :: IO ()
main =
  do input <- [format|8 (%s @C %d if %s %s %d%n)*|]
     let regmaps = scanl interpret mempty input
     print (maximum (last regmaps))
     print (maximumOf (folded . folded) regmaps)

-- | Lens for accessing a Map of Int values with a default
-- value of zero for missing entries.
register ::
   Ord k =>
   k                     {- ^ map key       -} ->
   Lens' (Map k Int) Int {- ^ lens into map -}
register r = at r . non 0

-- | Given registers and a statement, compute the resulting registers.
interpret ::
  Map String Int {- ^ incoming registers -} ->
  (String, C, Int, String, String, Int) {- ^ statement -} ->
  Map String Int {- ^ outgoing registers -}
interpret regs (r1,op1,n1,r2,op2,n2)
  | toCompare op2 (view (register r2) regs) n2 =
       over (register r1) (toArith op1 n1) regs
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

-- | Convert the string representation of an arithmetic operation to a function.
toArith ::
  C                   {- ^ name     -} ->
  (Int -> Int -> Int) {- ^ function -}
toArith Cinc = (+)
toArith Cdec = subtract
