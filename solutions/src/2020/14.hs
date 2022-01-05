{-# Language ImportQualifiedPost, QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/14>

@
>>> :set -XQuasiQuotes
>>> :{
let cmds = [format|- ((mask = @M*|mem[%u] = %u)%n)*|]
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
      \mem[8] = 11\n\
      \mem[7] = 101\n\
      \mem[8] = 0\n"
in run1 [] IntMap.empty cmds
:}
165

>>> :{
let cmds = [format|- ((mask = @M*|mem[%u] = %u)%n)*|]
      "mask = 000000000000000000000000000000X1001X\n\
      \mem[42] = 100\n\
      \mask = 00000000000000000000000000000000X0XX\n\
      \mem[26] = 1\n"
in run2 [] IntMap.empty cmds
:}
208

@

-}

module Main where

import Advent.Format(format)
import Data.Bits (setBit, clearBit)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (foldl')

type Cmd = Either [M] (Int,Int)
data M = M1 | M0 | MX deriving (Show)
pure[]

-- |
-- >>> :main
-- 17934269678453
-- 3440662844064
main :: IO ()
main =
  do inp <- [format|2020 14 ((mask = @M*|mem[%u] = %u)%n)*|]
     print (run1 [] IntMap.empty inp)
     print (run2 [] IntMap.empty inp)

-- | Simulate the computer using the 'mask1' rule.
run1 ::
  [M]        {- ^ initial mask       -} ->
  IntMap Int {- ^ initial memory     -} ->
  [Cmd]      {- ^ program statements -} ->
  Int
run1 _    mem []                 = sum mem
run1 _    mem (Left mask   : xs) = run1 mask mem xs
run1 mask mem (Right (k,v) : xs) = run1 mask mem' xs
  where
    mem' = IntMap.insert k v' mem
    v'   = mask1 v 35 mask

-- | Apply a mask where @1@ and @0@ overwrite bits.
--
-- >>> mask1 11 6 [M1,MX,MX,MX,MX,M0,MX]
-- 73
--
-- >>> mask1 101 6 [M1,MX,MX,MX,MX,M0,MX]
-- 101
--
-- >>> mask1 0 6 [M1,MX,MX,MX,MX,M0,MX]
-- 64
mask1 ::
  Int {- ^ target value                   -} ->
  Int {- ^ bit index of beginning of mask -} ->
  [M] -> Int
mask1 acc i (M1:xs) = mask1 (setBit   acc i) (i-1) xs
mask1 acc i (M0:xs) = mask1 (clearBit acc i) (i-1) xs
mask1 acc i (MX:xs) = mask1 acc              (i-1) xs
mask1 acc _ []     = acc

-- | Simulate the computer using the 'mask2' rule.
run2 ::
  [M]        {- ^ initial mask       -} ->
  IntMap Int {- ^ initial memory     -} ->
  [Cmd]      {- ^ program statements -} ->
  Int        {- ^ sum of memory      -}
run2 _    mem []                 = sum mem
run2 _    mem (Left mask   : xs) = run2 mask mem xs
run2 mask mem (Right (k,v) : xs) = run2 mask mem' xs
  where
    mem' = foldl' (\m_ k_ -> IntMap.insert k_ v m_) mem
         $ mask2 k 35 mask

-- | Apply a mask where 'I' overwrites and 'X' takes both bit values.
--
-- >>> mask2 42 5 [MX,M1,M0,M0,M1,MX]
-- [59,27,58,26]
mask2 ::
  Int {- ^ target value                   -} ->
  Int {- ^ bit index of beginning of mask -} ->
  [M] -> [Int]
mask2 x i (M1:xs) = mask2 (setBit x i) (i-1) xs
mask2 x i (M0:xs) = mask2 x (i-1) xs
mask2 x i (MX:xs) = do y <- mask2 (setBit x i) (i-1) xs; [y, clearBit y i]
mask2 x _ []      = [x]
