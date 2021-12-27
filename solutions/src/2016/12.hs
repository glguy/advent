{-# Language RankNTypes, ImportQualifiedPost, LambdaCase, ViewPatterns #-}
module Main where

import Advent ( getInputLines )
import AsmProg
import Control.Applicative (Alternative((<|>)))
import Control.Lens ((^.), (&~), (+=), (-=), (.=), (<~))
import Data.Foldable (for_)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Text.ParserCombinators.ReadP (ReadP, char, readP_to_S, string)

main :: IO ()
main =
  do program <- Vector.fromList . map parseInstruction <$> getInputLines 12
     print (execute program 0)
     print (execute program 1)

data Inst
  = Copy !Value !Register
  | Inc !Register
  | Dec !Register
  | Jnz !Value !Int
 deriving Show

parseInstruction :: String -> Inst
parseInstruction (readP_to_S pInst -> [(x,_)]) = x
parseInstruction x = error ("bad input: " ++ x)

pInst :: ReadP Inst
pInst =
  Copy <$ string "cpy " <*> pValue <* char ' ' <*> pReg <|>
  Jnz  <$ string "jnz " <*> pValue <* char ' ' <*> pInt <|>
  Inc  <$ string "inc " <*> pReg <|>
  Dec  <$ string "dec " <*> pReg

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
