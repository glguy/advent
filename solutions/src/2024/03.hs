{-# Language LambdaCase #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/3>

>>> :main + "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
161
48

-}
module Main (main) where

import Advent (getRawInput)
import Advent.ReadS (P, runP, pread, string, (<++), pany, psatisfy)
import Control.Applicative (asum, many, some, empty)
import Data.Char (isDigit)

-- | >>> :main
-- 166357705
-- 88811886
main :: IO ()
main =
 do input <- runP (many pInstr) <$> getRawInput 2024 3
    print (part1 input)
    print (part2 input)
 
part1 :: [Instr] -> Int
part1 xs = sum [x * y | Mul x y <- xs]

part2 :: [Instr] -> Int
part2 = snd . foldl f (True, 0)
  where
    f (enabled, acc) = \case
      Do            -> (True, acc)
      Don't         -> (False, acc)
      Mul x y
        | enabled   -> (enabled, acc + x * y)
        | otherwise -> (enabled, acc)

data Instr = Mul Int Int | Do | Don't
  deriving Show

-- | Parse a number consisting of 1 to 3 digits.
pInt :: P Int
pInt =
 do str <- some (psatisfy isDigit)
    if length str <= 3 then pure (read str) else empty

pInstr :: P Instr
pInstr = asum
  [ Mul   <$ string "mul(" <*> pInt <* string "," <*> pInt <* string ")"
  , Do    <$ string "do()"
  , Don't <$ string "don't()"
  ] <++ pany *> pInstr
