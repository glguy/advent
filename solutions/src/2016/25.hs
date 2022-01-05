{-# Language ViewPatterns, ImportQualifiedPost, MonoLocalBinds, TemplateHaskell, LambdaCase #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/25>

Assembly that can output binary numbers. We run them
until we can establish that the state is looping while
producing the desired output sequence.

-}
module Main where

import Advent.Input ( getInputLines )
import Advent.ReadS ( P(..), runP )
import AsmProg
import Control.Applicative ( Alternative(empty) )
import Control.Lens (use, (+=), (-=), (.=), (<~), makeLenses, Contains(contains))
import Control.Monad.Trans.State ( evalState )
import Data.List ( find )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector

-- | Expected next output
data Progress = NeedOne | NeedZero

-- | State of the interpreter
data Machine = Machine
  { _machRegisters :: !Registers
  , _machProgress  :: !Progress
  , _machTargets   :: !(Set (Int,Registers))
  }

makeLenses ''Machine

instance HasRegisters Machine where
  reg r = machRegisters . reg r
  {-# INLINE reg #-}

-- | >>> :main
-- Just 192
main :: IO ()
main =
 do program <- Vector.fromList . map (runP pInst) <$> getInputLines 2016 25
    print $ find (execute program) [1..]

data Inst
  = Copy Value Register
  | Inc Register
  | Dec Register
  | Jnz Value Value
  | Out Value
 deriving Show

pInst :: P Inst
pInst = P lex >>= \case
  "cpy" -> Copy <$> pValue <*> pReg
  "jnz" -> Jnz  <$> pValue <*> pValue
  "inc" -> Inc  <$> pReg
  "dec" -> Dec  <$> pReg
  "out" -> Out  <$> pValue
  _     -> empty

execute :: Vector Inst -> Int -> Bool
execute program a =
  evalState theMain (Machine zeroRegisters NeedZero mempty)
  where
    theMain = do reg A .= a
                 goto 0

    step pc = \case
      Out o ->
         do v <- rval o
            progress <- use machProgress
            case (progress, v) of
              (NeedOne,  1) ->
                  do machProgress .= NeedZero
                     goto (pc+1)

              (NeedZero, 0) ->
                  do registers <- use machRegisters
                     targets   <- use machTargets
                     let now = (pc,registers)
                     if Set.member now targets then
                       return True
                     else
                       do machTargets . contains now .= True
                          machProgress               .= NeedOne
                          goto (pc+1)

              _ -> return False

      Copy i o -> do reg o <~ rval i
                     goto (pc+1)

      Inc r    -> do reg r += 1
                     goto (pc+1)

      Dec r    -> do reg r -= 1
                     goto (pc+1)

      Jnz i o  -> do v  <- rval i
                     o' <- rval o
                     let pcOff = if v == 0 then 1 else o'
                     goto (pc+pcOff)

    goto pc =
      case program Vector.!? pc of
        Nothing -> return False
        Just o  -> step pc o
