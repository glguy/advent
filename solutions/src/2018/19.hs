{-# Language QuasiQuotes, TemplateHaskell, BlockArguments #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/19>

I finished part 2 with manual inspection, this only implements part 1.

-}
module Main (main) where

import           Advent               (format)
import           Data.Bits            ((.&.), (.|.))
import           Data.IntMap          (IntMap)
import qualified Data.IntMap.Strict          as IntMap
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector

data C = Caddi | Caddr | Cmuli | Cmulr | Cseti | Csetr
       | Cbani | Cbanr | Cbori | Cborr
       | Cgtir | Cgtri | Cgtrr
       | Ceqir | Ceqri | Ceqrr
  deriving (Eq, Ord, Show)

type Registers = IntMap Int

mempty

-- | Print the answers to day 19
main :: IO ()
main =
 do (ip, pgm) <- [format|2018 19 #ip %u%n(@C %u %u %u%n)*|]
    let regs = IntMap.fromList [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0)]
    print (run ip (Vector.fromList pgm) regs IntMap.! 0)

-- | Given a program counter register and a program, run the program
-- until the instruction pointer points outside of the program. The
-- final state of the registers is returned.
run :: Int -> Vector (C, Int, Int, Int) -> Registers -> Registers
run ip pgm regs =
  case pgm Vector.!? (regs IntMap.! ip) of
    Nothing -> regs
    Just (o, a, b, c)  -> run ip pgm (nextIP (opcodes o a b c regs))
  where
    nextIP = IntMap.update (Just . (1+)) ip

-- | Map from opcode names to opcode semantics. The functions expect
-- the operands A, B, and C as well as the current registers.
opcodes :: C -> Int -> Int -> Int -> Registers -> Registers
opcodes o = sem \reg a b ->
  case o of 
    Caddr -> reg a + reg b
    Caddi -> reg a + val b

    Cmulr -> reg a * reg b
    Cmuli -> reg a * val b

    Cbanr -> reg a .&. reg b
    Cbani -> reg a .&. val b

    Cborr -> reg a .|. reg b
    Cbori -> reg a .|. val b

    Csetr -> reg a
    Cseti -> val a

    Cgtir -> if val a > reg b then 1 else 0
    Cgtri -> if reg a > val b then 1 else 0
    Cgtrr -> if reg a > reg b then 1 else 0

    Ceqir -> if val a == reg b then 1 else 0
    Ceqri -> if reg a == val b then 1 else 0
    Ceqrr -> if reg a == reg b then 1 else 0
  where
    sem f a b c regs = IntMap.insert c (f (regs IntMap.!) a b) regs
    val v            = v
