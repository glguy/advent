{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost, ViewPatterns #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/10>

-}
module Main where

import Advent.Format (format)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map

data T = Tbot | Toutput deriving Show

mempty

-- | >>> :main
-- 147
-- 55637
main :: IO ()
main =
 do inp <- [format|2016 2016 10 ((value %u goes to @T %u|bot %u gives low to @T %u and high to @T %u)%n)*|]
    let solution = followInstructions (toInstr <$> inp)

    print (head [who | (Bot who, sort -> [17,61]) <- Map.toList solution])
    print (product [v | i <- [0..2], let [v] = solution Map.! Output i])

-- Types ---------------------------------------------------------------

data Instr = Value !Int !Target | Gives !Int !Target !Target
  deriving Show

data Target = Bot !Int | Output !Int
  deriving (Eq, Ord, Show)

-- Parsing -------------------------------------------------------------

toInstr :: Either (Int, T, Int) (Int, T, Int, T, Int) -> Instr
toInstr (Left (n, t, tn)) = Value n (toTarget t tn)
toInstr (Right (b, tlo, n, thi, m)) = Gives b (toTarget tlo n) (toTarget thi m)

toTarget :: T -> Int -> Target
toTarget Tbot = Bot
toTarget Toutput = Output

-- Solving -------------------------------------------------------------

followInstructions :: [Instr] -> Map Target [Int]
followInstructions xs = result
  where
    result = Map.fromListWith (++) (concatMap aux xs)

    aux (Value v tgt)     = [(tgt, [v])]
    aux (Gives src lo hi) = [(lo, [l]), (hi, [h])]
      where [l,h] = sort (result Map.! Bot src)
