{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost, LambdaCase #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/17>

-}
module Main (main) where

import Advent (format)
import Data.List (intercalate)
import Data.SBV
    (Word64, SWord64, SBool,
     (.==), (./=), (.&.), (.&&), sShiftRight, shiftR, xor,
     optLexicographic, free, minimize, constrain, getModelValue)

-- | >>> :main
-- 2,7,4,7,2,1,7,5,1
-- 37221274271220
main :: IO ()
main =
 do (a,b,c,program) <- [format|2024 17
      Register A: %u%n
      Register B: %u%n
      Register C: %u%n
      %n
      Program: %u&,%n|]

    putStrLn (intercalate "," (map show (run (Machine a b c) program)))

    res <- optLexicographic
      do a2 <- free "a"
         minimize "smallest" a2
         constrain (part2 a2 program)
    case getModelValue "a" res of
      Just x -> print (x :: Word64)
      Nothing -> fail "no solution"

data Machine = Machine { rA, rB, rC :: !Int }

run :: Machine -> [Int] -> [Int]
run m0 pgm = go m0 pgm
  where
    go m = \case
      0 : x : ip' -> go m{ rA = rA m `shiftR` combo x } ip'
      1 : x : ip' -> go m{ rB = rB m `xor`          x } ip'
      2 : x : ip' -> go m{ rB = combo x .&. 7         } ip'
      3 : x : ip' -> go m (if rA m == 0 then ip' else drop x pgm)
      4 : _ : ip' -> go m{ rB = rB m `xor`    rC m    } ip'
      5 : x : ip' -> combo x .&. 7 : go m ip'
      6 : x : ip' -> go m{ rB = rA m `shiftR` combo x } ip'
      7 : x : ip' -> go m{ rC = rA m `shiftR` combo x } ip'
      _           -> []
      where
        combo = \case
          0 -> 0; 1 -> 1; 2 -> 2; 3 -> 3
          4 -> rA m; 5 -> rB m; 6 -> rC m
          _ -> error "invalid combo operand"

part2 :: SWord64 -> [Int] -> SBool
part2 a []     = a .== 0
part2 a (o:os) = a ./= 0 .&& b2 .== fromIntegral o .&& part2 (a `shiftR` 3) os
  where
    b1 = a .&. 7 `xor` 2
    b2 = (a `sShiftRight` b1 `xor` b1 `xor` 3) .&. 7
