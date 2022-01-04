{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/21>

I copied my Day 19 solution and then modified it to check the state
of the running program.

Manually decompiling my input I find this C program:

@
#include \<stdio.h\>
#include \<stdlib.h\>
#include \<inttypes.h\>

static void program(uint32_t const r0) {
    uint32_t r5 = 0;
    do {
        r5 = 0xed43a1
           + 0x04dc53 * (0xff & r5)
           + 0xd802b9 * (0xff & (r5 >> 8))
           + 0x01016b * (1 | 0xff & (r5 >> 16));
        r5 &= 0xffffff;
        printf("r5 = %06" PRIx32 "\\n", r5);
    } while (r5 != r0);
}

int main(int argc, char **argv) {
    if (argc > 1) {
        program(atoi(argv[1]));
    }
}
@

This program generates a stream of 24-bit numbers and tests those
against register zero. This stream of numbers eventually repeats
itself. To find the shortest execution we must find the first
number generated in the cycle. To find the longest execution we must
find the last number generated.

-}
module Main (main) where

import Advent (format)
import Data.Bits ((.&.), (.|.))
import Data.Char (chr, ord)
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Vector (Vector)
import Data.Vector.Generic qualified as Vector
import Data.Vector.Generic.Mutable qualified as M
import Data.Vector.Unboxed qualified as U
import Numeric (showHex)


type Registers = U.Vector Int

data Instruction = I C !Int !Int !Int

data C = Caddi | Caddr | Cmuli | Cmulr | Cseti | Csetr
       | Cbani | Cbanr | Cbori | Cborr
       | Cgtir | Cgtri | Cgtrr
       | Ceqir | Ceqri | Ceqrr
  deriving (Eq, Ord, Show)

mempty

-- | Print the answers to day 21
--
-- >>> :main
-- 15615244
-- 12963935
main :: IO ()
main =
 do (ip, pgm') <- [format|19 #ip %u%n(@C %u %u %u%n)*|]
    let pgm = Vector.fromList [I o a b c | (o,a,b,c) <- pgm']
    let regs = Vector.replicate 6 0
    let xs = run ip (fmap semantics pgm) regs
    print (head xs)      -- part 1
    print (findCycle xs) -- part 2

-- | Find the last integer in the list that occurs before a repeated integer.
--
-- >>> findCycle [1,2,3,4,2]
-- 4
findCycle :: [Int] -> Int
findCycle = go 0 IntSet.empty
  where
    go answer seen (x:xs)
      | IntSet.member x seen = answer
      | otherwise = go x (IntSet.insert x seen) xs

-- | Given a program counter register and a program, run the program
-- until the instruction pointer points outside of the program.
-- The list of values of register 5 during program counter 29 is returned.
run :: Int -> Vector (Registers -> Registers) -> Registers -> [Int]
run ip pgm regs =
  case pgm Vector.!? pc of
    Nothing -> []
    Just f
      | pc == 29 -> regs Vector.! 5 : run ip pgm (nextIP (f regs))
      | otherwise -> run ip pgm (nextIP (f regs))
  where
    pc = Vector.unsafeIndex regs ip
    nextIP regs = chg regs ip (1+)

chg vec i f = U.modify (\v -> M.unsafeModify v f i) vec
set vec i e = U.modify (\v -> M.unsafeWrite v i e) vec

-- | Map from opcode names to opcode semantics. The functions expect
-- the operands A, B, and C as well as the current registers.
semantics :: Instruction -> Registers -> Registers
semantics (I op a b c) = sem $ \reg ->
  case op of
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
    sem f regs = set regs c $! f (Vector.unsafeIndex regs)
    val = id
