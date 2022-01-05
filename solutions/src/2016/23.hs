{-# Language ImportQualifiedPost, TemplateHaskell, LambdaCase #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/23>

-}
module Main where

import Advent ( getInputLines )
import Advent.ReadS ( P(..), runP )
import AsmProg
import Control.Lens
import Control.Applicative (Alternative((<|>), empty))
import Control.Monad.Trans.State.Strict ( evalState, State )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector

data Inst
  = Copy Value !Register
  | Inc !Register
  | Dec !Register
  | Jnz Value Value
  | Tgl Value
 deriving Show

data Machine = Machine
  { _machRegisters :: !Registers
  , _machProgram   :: !(Vector Inst)
  }

makeLenses '' Machine

instance HasRegisters Machine where
  reg r = machRegisters . reg r
  {-# INLINE reg #-}

-- | >>> :main
-- 13050
-- 479009610
main :: IO ()
main =
  do program <- Vector.fromList . map (runP pInst) <$> getInputLines 2016 23
     print (execute program 7)
     print (execute program 12)

pInst :: P Inst
pInst = P lex >>= \case
  "cpy" -> Copy <$> pValue <*> pReg
  "jnz" -> Jnz  <$> pValue <*> pValue
  "tgl" -> Tgl  <$> pValue
  "inc" -> Inc  <$> pReg
  "dec" -> Dec  <$> pReg
  _     -> empty

execute :: Vector Inst -> Int -> Int
execute program0 a =
  evalState mainEntry (Machine zeroRegisters program0)
  where
    mainEntry =
      do reg A .= a
         goto 0

    step pc = \case
      Copy i o -> (reg o <~ rval i) >> goto (pc+1)
      Inc r    -> (reg r += 1)      >> goto (pc+1)
      Dec r    -> (reg r -= 1)      >> goto (pc+1)
      Tgl r    -> do v <- rval r
                     toggle (pc+v)
                     goto (pc+1)
      Jnz i o  -> do v  <- rval i
                     o' <- rval o
                     goto (if v == 0 then pc+1 else pc+o')

    toggle :: Int -> State Machine ()
    toggle pc =
      machProgram . ix pc %= \case
        Inc x         -> Dec x
        Dec x         -> Inc x
        Tgl   (Reg x) -> Inc x
        Jnz x (Reg y) -> Copy x y
        Copy x y      -> Jnz x (Reg y)
        oper -> error ("Nonsense toggle: " ++ show pc ++ " " ++ show oper)

    goto pc =
     do program <- use machProgram
        case program Vector.!? pc of
          Just o -> step pc o
          Nothing -> use (reg A)
