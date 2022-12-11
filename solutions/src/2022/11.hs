{-# Language QuasiQuotes, ImportQualifiedPost, NumericUnderscores #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/11>

This solution relies on the fact that there are no interactions between
items, so each item can be simulated separately. When a monkey throws an
item to a monkey with a larger ID that monkey will get to throw the item
in the same round, but when it throws it to an earlier monkey, that item
will not be thrown until the next round. Using this we can keep track of
when an item would stop moving.

-}
module Main where

import Data.Array.Unboxed ( Array, (!), array )
import Data.Foldable (toList)
import Data.List (sortBy)

import Advent (format, counts)

-- | Input file contains a list of:
--
-- * Monkey ID
-- * Starting items
-- * @+@ or @*@
-- * A literal or the @old@ variable
-- * A divisor
-- * The monkey ID when the divisor divides the value
-- * The monkey ID when the divisor does not divide the value
type Input = [(Int, [Int], Char, Maybe Int, Int, Int, Int)]

main :: IO ()
main =
 do input <- [format|2022 11
      (Monkey %u:%n
        Starting items: %u&(, )%n
        Operation: new = old %c (old|%u)%n
        Test: divisible by %u%n
          If true: throw to monkey %u%n
          If false: throw to monkey %u%n)&%n|]
    
    -- It's safe to work with integers mod the lcm of all the divisor tests.
    -- This will keep the worry numbers small while preserving all the
    -- divisibility tests.
    let modulus = foldl1 lcm [d | (_, _, _, _, d, _, _) <- input]

    print (solve (`div`      3)     20 input)
    print (solve (`mod`modulus) 10_000 input)

-- | Run the given number of monkey throwing rounds and compute monkey business.
solve ::
  (Int -> Int) {- ^ extra operation to run after each monkey updates worry -} ->
  Int          {- ^ number of rounds to run -} ->
  Input        {- ^ input file contents -} ->
  Int          {- ^ product of top 2 counts of times each monkey threw something -}
solve post rounds input = product . top 2 . toList . counts $
  [ thrower
    | (i, startingItems, _, _, _, _, _) <- input
    , item <- startingItems
    , thrower <- go rounds i item
  ]
  where
    -- list of monkey ids used to find bounds for monkey array
    ids = [i | (i, _, _, _, _, _, _) <- input]

    -- store monkey information in array for faster random access
    monkeys :: Array Int (Int -> Int, Int, Int, Int)
    monkeys = array (minimum ids, maximum ids) [(i, (eval o n,d,t,f)) | (i, _, o, n, d, t, f) <- input]

    go :: Int -> Int -> Int -> [Int]
    go 0 _ _ = []
    go r i x =
      i :
      case monkeys ! i of
        (o,d,t,f) -> go r' i' x'
          where
            x' = post (o x)
            i' = if x' `mod` d == 0 then t else f
            r' = if i' < i then r-1 else r

-- | Evaluate a monkey's worry update function.
eval :: Char -> Maybe Int -> Int -> Int
eval '*' (Just n) = (n*)
eval '+' (Just n) = (n+)
eval '*' Nothing  = (^(2::Int))
eval '+' Nothing  = (2*)
eval op _ = error ("Unexpected operation: " ++ [op])

-- | Returns the largest @n@ numbers in a list
top :: Ord a => Int -> [a] -> [a]
top n = take n . sortBy (flip compare)