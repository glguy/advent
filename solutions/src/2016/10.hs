{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost #-}
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
import Data.Map (Map)
import Data.Map qualified as Map

data T = Tbot | Toutput deriving Show

mempty

main :: IO ()
main =
  do inp <- [format|10 ((value %u goes to @T %u|bot %u gives low to @T %u and high to @T %u)%n)*|]
     let solution = followInstructions (toInstr <$> inp)

     print (head [ who | (Bot who,Two 17 61) <- Map.toList solution])

     print $ product [ v | i <- [0..2]
                         , let One v = solution Map.! Output i ]

-- Types ---------------------------------------------------------------

data Instr = Value !Int !Target | Gives !Int !Target !Target
  deriving Show

data Target = Bot !Int | Output !Int
  deriving (Eq, Ord, Show)

data Holding = One !Int | Two !Int !Int
  deriving Show

-- Parsing -------------------------------------------------------------

toInstr :: Either (Int, T, Int) (Int, T, Int, T, Int) -> Instr
toInstr (Left (n, t, tn)) = Value n (toTarget t tn)
toInstr (Right (b, tlo, n, thi, m)) = Gives b (toTarget tlo n) (toTarget thi m)

toTarget :: T -> Int -> Target
toTarget Tbot = Bot
toTarget Toutput = Output

-- Solving -------------------------------------------------------------

followInstructions :: [Instr] -> Map Target Holding
followInstructions xs = result
  where
    result = Map.fromListWithKey combine (concatMap aux xs)

    aux (Value val tgt)   = [ (tgt, One val) ]
    aux (Gives src lo hi) = [ (lo , One l), (hi, One h) ]
      where Two l h = result Map.! Bot src


combine :: Target -> Holding -> Holding -> Holding
combine _ (One x) (One y) = Two (min x y) (max x y)
combine tgt _ _ = error ("Bad combination for " ++ show tgt)
