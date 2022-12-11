{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/11>

-}
module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.List ( sort )

import Advent ( format, times )
import Advent.Queue qualified as Queue
import Advent.Queue (Queue)

data Monkey = Monkey {
  items :: Queue Int,
  fun :: Int -> Int,
  divisor :: Int,
  onTrue :: Int,
  onFalse :: Int,
  throws :: Int
}

main :: IO ()
main = do
    input <- [format|2022 11
      (Monkey %u:%n
        Starting items: %u&(, )%n
        Operation: new = old %c (old|%u)%n
        Test: divisible by %u%n
          If true: throw to monkey %u%n
          If false: throw to monkey %u%n)&%n|]

    let monkeys = Map.fromList
          [(i, Monkey (Queue.fromList start) (eval op arg) d t f 0)
          | (i, start, op, arg, d, t, f) <- input]
    

    print $ product $ take 2 $ reverse $ sort $ Map.elems $ fmap throws $ times 20 (runRound Nothing) monkeys
    let magic = foldl1 lcm [d | (_, _, _, _, d, _, _) <- input]
    print $ product $ take 2 $ reverse $ sort $ Map.elems $ fmap throws $ times 10000 (runRound (Just magic)) monkeys

eval :: Char -> Maybe Int -> Int -> Int
eval '*' (Just n) x  = x * n
eval '+' (Just n) x  = x + n
eval '*' Nothing  x = x * x
eval '+' Nothing  x = x + x
eval op _ _ = error ("Unexpected operation: " ++ [op])

runRound :: Maybe Int -> Map Int Monkey -> Map Int Monkey
runRound magic ms =
  foldl (runMonkey magic) ms (Map.keys ms)

runMonkey :: Maybe Int -> Map Int Monkey -> Int ->  Map Int Monkey
runMonkey magic ms i =
  let m = ms Map.! i in
  case items m of
    Queue.Empty -> ms
    item Queue.:<| items' ->
      let val = case magic of Just modulus -> fun m item `mod` modulus
                              Nothing ->  fun m item `div` 3
          target = if 0 == val `mod` fromIntegral (divisor m) then onTrue m else onFalse m
      in
      runMonkey magic
      (Map.adjust (push val) target $
      Map.adjust (\x -> x{items = items', throws=throws m + 1}) i ms) i

push :: Int -> Monkey -> Monkey
push x m = m { items = Queue.snoc x (items m)}