{-# Language LambdaCase #-}
{-|
Module      : Advent.AsmProg
Description : Support module for 2016
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

This module supports a set of 3 tasks from the 2016 problem set that
used a 4 register machine state.

-}
module Advent.AsmProg where

import Advent.ReadS (P(..))
import Control.Applicative ( Alternative(empty, (<|>)) )
import Control.Lens (use)
import Control.Monad.State ( MonadState )

data Registers = Registers { _regA, _regB, _regC, _regD :: !Int }
  deriving (Read, Show, Eq, Ord)

regA :: Functor f => (Int -> f Int) -> Registers -> f Registers
regA f r = (\x -> r { _regA = x}) <$> f (_regA r)

regB :: Functor f => (Int -> f Int) -> Registers -> f Registers
regB f r = (\x -> r { _regB = x}) <$> f (_regB r)

regC :: Functor f => (Int -> f Int) -> Registers -> f Registers
regC f r = (\x -> r { _regC = x}) <$> f (_regC r)

regD :: Functor f => (Int -> f Int) -> Registers -> f Registers
regD f r = (\x -> r { _regD = x}) <$> f (_regD r)

zeroRegisters :: Registers
zeroRegisters = Registers 0 0 0 0

class HasRegisters a where
  reg :: Functor f => Register -> (Int -> f Int) -> a -> f a

data Register = A | B | C | D
  deriving (Show, Eq, Ord)

instance HasRegisters Registers where
  reg A = regA
  reg B = regB
  reg C = regC
  reg D = regD
  {-# INLINE reg #-}

data Value = Int !Int | Reg !Register
  deriving Show

rval :: (MonadState r m, HasRegisters r) => Value -> m Int
rval = \case
  Int i -> pure i
  Reg r -> use (reg r)
{-# INLINE rval #-}

pValue :: P Value
pValue = Int <$> P reads <|> Reg <$> pReg

pReg :: P Register
pReg = P lex >>= \case
  "a" -> pure A
  "b" -> pure B
  "c" -> pure C
  "d" -> pure D
  _   -> empty
