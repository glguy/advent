{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.Bits
import Data.Map (Map)
import Data.Word
import Text.Read (readMaybe)
import qualified Data.Map as Map

data Gate a = Gate1 Op1 a | Gate2 Op2 a a deriving Functor
data Op1 = Not | Id
data Op2 = And | Or | LShift | RShift

main :: IO ()
main =
  do circuit1 <- loadInput
     let answer1 = findAnswer circuit1
     print answer1

     let circuit2 = Map.insert "b" (Gate1 Id (show answer1)) circuit1
     print (findAnswer circuit2)

loadInput :: IO (Map String (Gate String))
loadInput = parseLines <$> readFile "input7.txt"

-- | Build a circuit and compute output 'a'
findAnswer :: Map String (Gate String) -> Word16
findAnswer circuit = tieCircuit circuit Map.! "a"

tieCircuit :: Map String (Gate String) -> Map String Word16
tieCircuit m = m'
  where
  m' = fmap (evalGate . fmap evalKey) m

  evalKey key
    | Just n <- readMaybe key = n
    | otherwise               = m' Map.! key

evalGate :: Gate Word16 -> Word16
evalGate (Gate1 Id     x  ) = x
evalGate (Gate1 Not    x  ) = complement x
evalGate (Gate2 And    x y) = x .&. y
evalGate (Gate2 Or     x y) = x .|. y
evalGate (Gate2 RShift x y) = x `shiftR` fromIntegral y
evalGate (Gate2 LShift x y) = x `shiftL` fromIntegral y

parseLines :: String -> Map String (Gate String)
parseLines = Map.fromList . map parseLine . lines

-- | Parse a line describing a gate in the circuit.
parseLine :: String -> (String, Gate String)
parseLine cmd =
  case words cmd of
    [x,           "->",y] -> (y, Gate1 Id x)
    ["NOT",x,     "->",y] -> (y, Gate1 Not x)
    [x,"AND",   y,"->",z] -> (z, Gate2 And x y)
    [x,"OR",    y,"->",z] -> (z, Gate2 Or x y)
    [x,"LSHIFT",y,"->",z] -> (z, Gate2 LShift x y)
    [x,"RSHIFT",y,"->",z] -> (z, Gate2 RShift x y)
    _                     -> error ("parseLine: " ++ cmd)
