{-# Language RankNTypes, ImportQualifiedPost, LambdaCase, ViewPatterns #-}
module Main where

import Advent
import Control.Lens
import Control.Applicative
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Map (Map)
import Data.Vector qualified as Vector
import Data.Vector ( Vector )
import Text.ParserCombinators.ReadP
import Data.Char

main :: IO ()
main =
  do program <- Vector.fromList . map parseInstruction <$> getInputLines 12
     print (execute program 0)
     print (execute program 1)

data Value = Int !Int | Reg Char deriving (Show)

data Inst
  = Copy Value !Char
  | Inc !Char
  | Dec !Char
  | Jnz Value !Int
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

pInt :: ReadP Int
pInt = read <$> ((++) <$> option "" (string "-") <*> munch1 isDigit)

pValue :: ReadP Value
pValue = Reg <$> pReg <|> Int <$> pInt

pReg :: ReadP Char
pReg = satisfy isAlpha

reg :: Char -> Lens' (Map Char Int) Int
reg x = at x . non 0

execute :: Vector Inst -> Int -> Int
execute program c = (Map.singleton 'c' c &~ goto 0) ^. reg 'a'
  where
    step = \case
      Copy i o -> 1 <$ (reg o <~ rval i)
      Inc r    -> 1 <$ (reg r += 1)
      Dec r    -> 1 <$ (reg r -= 1)
      Jnz i o  -> do v <- rval i
                     return $! if v == 0 then 1 else o

    rval = \case
      Int i -> pure i
      Reg r -> use (reg r)

    goto pc =
      for_ (program Vector.!? pc) $ \o ->
        do offset <- step o
           goto (pc + offset)
