{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/7>

In order to prune out a lot of the search space, this solution works
"backwards" from the right. At this point we'll know which of the
operations potentially valid and which were not allowing the search
space to be much smaller.

This solution assumes that all inputs are greater than 0.

>>> :{
:main + "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"
:}
3749
11387

-}
module Main (main) where

import Advent (format)
import Data.Foldable (foldrM)

-- | >>> :main
-- 6231007345478
-- 333027885676693
main :: IO ()
main =
 do input <- [format|2024 7 (%u: %u& %n)*|]
    print (sum [tgt | (tgt, ns) <- input, isReachable [addOp, mulOp       ] tgt ns])
    print (sum [tgt | (tgt, ns) <- input, isReachable [addOp, mulOp, catOp] tgt ns])

-- | Reversed operations that attempt to cancel out an effect
type Op = Int -> Int -> [Int]

-- | Cancel out an addition
--
-- >>> addOp 7 10
-- [3]
--
-- >>> addOp 10 7
-- []
addOp :: Op
addOp a b = [b - a | b > a]

-- | Cancel out a multiplication
--
-- >>> mulOp 3 15
-- [5]
--
-- >>> mulOp 3 16
-- []
mulOp :: Op
mulOp a b = [q | let (q, r) = b `quotRem` a, q > 0, r == 0]

-- | Cancel out a concatenation.
--
-- >>> catOp 12 4512
-- [45]
--
-- >>> catOp 12 1245
-- []
catOp :: Op
catOp 0 b = [b | b > 0]
catOp a b | (qa,ra) <- quotRem a 10, (qb,rb) <- quotRem b 10, ra == rb = catOp qa qb
catOp _ _ = []

-- | Try to combine the input numbers using the available operations
-- to reach the target number.
isReachable ::
    [Op]  {- ^ available operations -} ->
    Int   {- ^ target               -} ->
    [Int] {- ^ inputs               -} ->
    Bool  {- ^ target is reachable  -}
isReachable ops target [] = error "Input sequence requires at least one number"
isReachable ops target (n:ns) = n `elem` foldrM tryOps target ns
  where
    tryOps a b = [r | op <- ops, r <- a `op` b]
