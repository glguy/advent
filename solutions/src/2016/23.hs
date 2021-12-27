{-# Language ImportQualifiedPost, ViewPatterns, TemplateHaskell, LambdaCase #-}
module Main where

import Advent
import AsmProg
import Control.Lens
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Text.ParserCombinators.ReadP

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

main :: IO ()
main =
  do program <- Vector.fromList . map parseLine <$> getInputLines 23
     print (execute program 7)
     print (execute program 12)

parseLine :: String -> Inst
parseLine (readP_to_S pInst -> [(x,_)]) = x

pInst :: ReadP Inst
pInst =
  Copy <$ string "cpy " <*> pValue <* char ' ' <*> pReg <|>
  Jnz  <$ string "jnz " <*> pValue <* char ' ' <*> pValue <|>
  Tgl  <$ string "tgl " <*> pValue <|>
  Inc  <$ string "inc " <*> pReg <|>
  Dec  <$ string "dec " <*> pReg

execute :: Vector Inst -> Int -> Int
execute program0 a =
  evalState mainEntry (Machine zeroRegisters program0)
  where
    mainEntry =
      do reg A .= a
         goto 0

    step pc o =
      case o of
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
      machProgram . ix pc %= \oper ->
        case oper of
          Inc x         -> Dec x
          Dec x         -> Inc x
          Tgl   (Reg x) -> Inc x
          Jnz x (Reg y) -> Copy x y
          Copy x y      -> Jnz x (Reg y)
          _ -> error ("Nonsense toggle: " ++ show pc ++ " " ++ show oper)

    goto pc =
     do program <- use machProgram
        case program Vector.!? pc of
          Just o -> step pc o
          Nothing -> use (reg A)
