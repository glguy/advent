{-# Language BangPatterns, LambdaCase, ViewPatterns, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/24>

Solve an arbitrary program using an SMT solver.

-}
module Main (main) where

import Advent.Input ( getInputLines )
import Advent.ReadS ( P(..), runP )
import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad ( foldM )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe ( fromJust, isJust )
import Data.SBV

-- | >>> :main
-- 49917929934999
-- 11911316711816
main :: IO ()
main =
 do pgm <- map (runP pOp) <$> getInputLines 24
    print =<< findAnswer maximize pgm
    print =<< findAnswer minimize pgm

findAnswer :: Opt -> [Op] -> IO String
findAnswer opt pgm =
 do LexicographicResult r <- optimize Lexicographic (runProgram opt pgm)
    pure $
      concatMap (show . fromJust) $
      takeWhile isJust
      [getModelValue ("in"++show i) r :: Maybe Int64 | i <- [1::Int ..]]

-- * Programs

-- | Program opcodes
data Op = Inp Reg | Mul Reg Term | Add Reg Term | Div Reg Term | Mod Reg Term | Eql Reg Term
  deriving Show

-- | Machine registers
data Reg = W | X | Y | Z
  deriving (Eq, Ord, Show)

-- | Opcode terms that can evaluate to an integer
data Term = Reg Reg | Int Int64
  deriving Show

-- * Interpretation

-- | Optimization function ('minimize' or 'maximize')
type Opt = String -> SBV Int64 -> Symbolic ()

-- | Map of values associated with each register
type Regs = Map Reg SInt64

-- | Evaluate a 'Term'
val :: Regs -> Term -> SInt64
val regs (Reg r) = regs Map.! r
val _    (Int i) = literal i

-- | Optimize the given program ensuring it ends with @z == 0@
runProgram :: Opt -> [Op] -> Symbolic ()
runProgram opt pgm =
 do let regs = Map.fromList [(W,0),(X,0),(Y,0),(Z,0)]
    (_, regs') <- foldM (step opt) (1, regs) pgm
    namedConstraint "z==0" (regs' Map.! Z .== 0)

step :: Opt -> (Int, Regs) -> Op -> Symbolic (Int, Regs)
step opt (n, regs) (Inp r) =
 do i <- free ("in" ++ show n)
    opt "opt" i
    constrain (inRange i (1,9))
    pure (n+1, Map.insert r i regs)
step _ (n, regs) (Add x y) = pure (n, Map.adjust (val regs y +)                     x regs)
step _ (n, regs) (Mul x y) = pure (n, Map.adjust (val regs y *)                     x regs)
step _ (n, regs) (Div x y) = pure (n, Map.adjust (`sDiv` val regs y)                x regs)
step _ (n, regs) (Mod x y) = pure (n, Map.adjust (`sMod` val regs y)                x regs)
step _ (n, regs) (Eql x y) = pure (n, Map.adjust (\a -> ite (a .== val regs y) 1 0) x regs)

-- * Parsing

pOp :: P Op
pOp = P lex >>= \case
  "inp" -> Inp <$> pReg
  "add" -> Add <$> pReg <*> pTerm
  "mul" -> Mul <$> pReg <*> pTerm
  "div" -> Div <$> pReg <*> pTerm
  "mod" -> Mod <$> pReg <*> pTerm
  "eql" -> Eql <$> pReg <*> pTerm
  _     -> empty

pReg :: P Reg
pReg = P lex >>= \case
  "w" -> pure W
  "x" -> pure X
  "y" -> pure Y
  "z" -> pure Z
  _   -> empty

pTerm :: P Term
pTerm = Int <$> P reads <|> Reg <$> pReg
