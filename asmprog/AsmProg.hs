{-# Language TemplateHaskell #-}
module AsmProg where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Text.ParserCombinators.ReadP
import Data.Char

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

pInt :: ReadP Int
pInt = read <$> ((++) <$> option "" (string "-") <*> munch1 isDigit)

pValue :: ReadP Value
pValue = Int <$> pInt <|> Reg <$> pReg

pReg :: ReadP Register
pReg = choice
  [ A <$ char 'a'
  , B <$ char 'b'
  , C <$ char 'c'
  , D <$ char 'd' ]
