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
     (.==), (.&.), (.&&), sShiftRight, shiftR, xor,
     optLexicographic, free, minimize, constrain, getModelValue, sFalse, symbolicMerge, fromBool)

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
         constrain (run2 (SMachine program a2 0 0) program)
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
      2 : x : ip' -> go m{ rB = 7    .&.      combo x } ip'
      4 : _ : ip' -> go m{ rB = rB m `xor`    rC m    } ip'
      6 : x : ip' -> go m{ rB = rA m `shiftR` combo x } ip'
      7 : x : ip' -> go m{ rC = rA m `shiftR` combo x } ip'
      3 : x : ip' -> go m (if rA m == 0 then ip' else drop x pgm)
      5 : x : ip' -> combo x .&. 7 : go m ip'
      _           -> []
      where
        combo = \case
          0 -> 0; 1 -> 1; 2 -> 2; 3 -> 3
          4 -> rA m; 5 -> rB m; 6 -> rC m
          _ -> error "invalid combo operand"

data SMachine = SMachine { outs :: [Int], sA, sB, sC :: SWord64 }

run2 :: SMachine -> [Int] -> SBool
run2 m0 pgm = go m0 pgm
  where
    go m = \case
      0 : x : ip' -> go m{ sA = sA m `sShiftRight` combo x } ip'
      1 : x : ip' -> go m{ sB = sB m `xor`  fromIntegral x } ip'
      2 : x : ip' -> go m{ sB = 7    .&.           combo x } ip'
      4 : _ : ip' -> go m{ sB = sB m `xor`         sC m    } ip'
      6 : x : ip' -> go m{ sB = sA m `sShiftRight` combo x } ip'
      7 : x : ip' -> go m{ sC = sA m `sShiftRight` combo x } ip'
      3 : x : ip' -> symbolicMerge False
                       (sA m .== 0) (go m ip') (go m (drop x pgm))
      5 : x : ip' ->
        case outs m of
          []   -> sFalse
          o:os -> combo x .&. 7 .== fromIntegral o .&& go m{ outs = os} ip'
      _ -> fromBool (null (outs m))
      where
        combo = \case
          0 -> 0; 1 -> 1; 2 -> 2; 3 -> 3
          4 -> sA m; 5 -> sB m; 6 -> sC m
          _ -> error "invalid combo operand"
