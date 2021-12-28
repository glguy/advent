{-# Language TemplateHaskell, LambdaCase #-}
module AsmProg where

import Advent.ReadS ( P(..) )
import Control.Applicative ( Alternative(empty, (<|>)) )
import Control.Lens ( LensLike', use, makeLenses )
import Control.Monad.State ( MonadState )

data Registers = Registers { _regA, _regB, _regC, _regD :: !Int }
  deriving (Read, Show, Eq, Ord)

makeLenses ''Registers

zeroRegisters :: Registers
zeroRegisters = Registers 0 0 0 0

class HasRegisters a where
  reg :: Functor f => Register -> LensLike' f a Int

data Register = A|B|C|D
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
rval v =
  case v of
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
