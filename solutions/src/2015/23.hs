{-# Language BangPatterns, LambdaCase, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/23>

We're given a program that computes Collatz conjecture in a
purpose-built assembly language.

-}
module Main (main) where

import Advent (getInputLines, arrIx)
import Advent.ReadS
import Control.Applicative (empty, optional)
import Data.Array (Array, listArray)

-- | >>> :main
-- 255
-- 334
main :: IO ()
main =
  do pgm <- toArray . map (runP pInstr) <$> getInputLines 2015 23
     print (program pgm 0 0)
     print (program pgm 1 0)

-- | Turn a list into a zero-indexed array
toArray :: [a] -> Array Int a
toArray xs = listArray (0,length xs - 1) xs

-- | Run a program to completion starting from the first instruction.
program ::
  Array Int Instr {- ^ program -} ->
  Int {- ^ initial a register -} ->
  Int {- ^ initial b register -} ->
  Int {- ^ final b register -}
program pgm = loop 0
  where
    loop !i !a !b =
      case arrIx pgm i of
        Nothing -> b
        Just instr ->
          case instr of
            Hlf A   -> loop (i+1) (a`quot`2) b
            Hlf B   -> loop (i+1) a (b`quot`2)
            Tpl A   -> loop (i+1) (3*a) b
            Tpl B   -> loop (i+1) a (3*b)
            Inc A   -> loop (i+1) (a+1) b
            Inc B   -> loop (i+1) a (b+1)
            Jmp o   -> loop (i+o) a b
            Jie A o -> loop (i+if even a then o else 1) a b
            Jie B o -> loop (i+if even b then o else 1) a b
            Jio A o -> loop (i+if a == 1 then o else 1) a b
            Jio B o -> loop (i+if b == 1 then o else 1) a b

-- * Program data type

-- | A program instruction
data Instr
  = Hlf Register -- ^ divide the register's contents by 2
  | Tpl Register -- ^ multiply the register's contents by 3
  | Inc Register -- ^ increment the register
  | Jmp Int -- ^ jump to a fixed offset
  | Jie Register Int -- ^ jump to a fixed offset when the register is even
  | Jio Register Int -- ^ jump to a fixed offset when the register is odd
  deriving Show

-- | A program register
data Register = A | B
  deriving Show

-- * Parsing

-- | Parse a single instruction
pInstr :: P Instr
pInstr = P lex >>= \case
  "hlf" -> Hlf <$> pReg
  "tpl" -> Tpl <$> pReg
  "inc" -> Inc <$> pReg
  "jmp" -> Jmp <$> pOffset
  "jie" -> Jie <$> pReg <* "," <*> pOffset
  "jio" -> Jio <$> pReg <* "," <*> pOffset
  _     -> empty

-- | Parse a jump offset
pOffset :: P Int
pOffset = optional "+" *> P reads

-- | Parse a register a or b
pReg :: P Register
pReg = P lex >>= \case
  "a" -> pure A
  "b" -> pure B
  _   -> empty
