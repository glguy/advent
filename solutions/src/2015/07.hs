{-# LANGUAGE ImportQualifiedPost, DeriveFunctor, OverloadedStrings #-}
module Main where

import Advent (getInputLines)
import Advent.ReadS (P(..), runP, (<++))
import Control.Applicative ((<|>))
import Data.Bits (Bits(shiftL, complement, (.&.), (.|.), shiftR))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word16)

data Gate a = Gate1 Op1 a | Gate2 a Op2 a deriving Functor
data Op1 = Not | Id
data Op2 = And | Or | LShift | RShift

main :: IO ()
main =
 do circuit1 <- parse <$> getInputLines 2015 7
    let answer1 = findAnswer circuit1
    print answer1

    let circuit2 = Map.insert "b" (Gate1 Id (Left answer1)) circuit1
    print (findAnswer circuit2)

-- | Build a circuit and compute output 'a'
findAnswer :: Map String (Gate (Either Word16 String)) -> Word16
findAnswer circuit = tieCircuit circuit Map.! "a"

tieCircuit :: Map String (Gate (Either Word16 String)) -> Map String Word16
tieCircuit m = m'
  where
  m' = fmap (evalGate . fmap evalKey) m

  evalKey (Left lit) = lit
  evalKey (Right var) = m' Map.! var

evalGate :: Gate Word16 -> Word16
evalGate (Gate1   Id     x) = x
evalGate (Gate1   Not    x) = complement x
evalGate (Gate2 x And    y) = x .&. y
evalGate (Gate2 x Or     y) = x .|. y
evalGate (Gate2 x RShift y) = x `shiftR` fromIntegral y
evalGate (Gate2 x LShift y) = x `shiftL` fromIntegral y

-- * Parsing

parse :: [String] -> Map String (Gate (Either Word16 String))
parse = Map.fromList . map (runP pCmd)

pCmd :: P (String, Gate (Either Word16 String))
pCmd = flip (,) <$> pGate <* "->" <*> P lex

pGate :: P (Gate (Either Word16 String))
pGate =
  Gate1 <$>          pOp1 <*> pArg <|>
  Gate2 <$> pArg <*> pOp2 <*> pArg

pOp1 :: P Op1
pOp1 = pure Id <|> Not <$ "NOT"

pOp2 :: P Op2
pOp2 = And <$ "AND" <|> Or <$ "OR" <|> LShift <$ "LSHIFT" <|> RShift <$ "RSHIFT"

pArg :: P (Either Word16 String)
pArg = Left <$> P reads <++ Right <$> P lex
