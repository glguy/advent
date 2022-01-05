{-# Language RankNTypes, ImportQualifiedPost, LambdaCase #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/12>

-}
module Main where

import Advent (getInputLines)
import Advent.ReadS
import AsmProg
import Control.Applicative (Alternative((<|>), empty))
import Control.Lens ((^.), (&~), (+=), (-=), (.=), (<~))
import Data.Foldable (for_)
import Data.Vector (Vector)
import Data.Vector qualified as Vector

-- | >>> :main
-- 318077
-- 9227731
main :: IO ()
main =
  do program <- Vector.fromList . map (runP pInst) <$> getInputLines 2016 12
     print (execute program 0)
     print (execute program 1)

data Inst
  = Copy !Value !Register
  | Inc !Register
  | Dec !Register
  | Jnz !Value !Int
 deriving Show

pInst :: P Inst
pInst = P lex >>= \case
  "cpy" -> Copy <$> pValue <*> pReg
  "jnz" -> Jnz  <$> pValue <*> P reads
  "inc" -> Inc  <$> pReg
  "dec" -> Dec  <$> pReg
  _     -> empty

execute :: Vector Inst -> Int -> Int
execute program c = (zeroRegisters &~ entry) ^. reg A
  where
    entry =
     do reg C .= c
        goto 0

    step = \case
      Copy i o -> 1 <$ (reg o <~ rval i)
      Inc r    -> 1 <$ (reg r += 1)
      Dec r    -> 1 <$ (reg r -= 1)
      Jnz i o  -> do v <- rval i
                     return $! if v == 0 then 1 else o

    goto pc =
      for_ (program Vector.!? pc) $ \o ->
        do offset <- step o
           goto (pc + offset)
