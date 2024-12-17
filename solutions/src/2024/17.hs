{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost, LambdaCase #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/17>

>>> :{
:main + "Register A: 729
Register B: 0
Register C: 0
\&
Program: 0,1,5,4,3,0
"
:}
4,6,3,5,6,3,5,2,1,0
no part 2

>>> :{
:main + "Register A: 2024
Register B: 0
Register C: 0
\&
Program: 0,3,5,4,3,0
"
:}
5,7,3,0
117440

-}
module Main (main) where

import Advent (format)
import Data.List (intercalate, isSuffixOf)
import Data.SBV
    (Word64, SWord64, SBool,
     (.==), (.&.), (.&&), sShiftRight, shiftR, xor,
     optLexicographic, minimize, constrain, getModelValue, sFalse, ite, fromBool)

-- | >>> :main
-- 2,7,4,7,2,1,7,5,1
-- 37221274271220
main :: IO ()
main =
 do (a1,b,c,program) <- [format|2024 17
      Register A: %u%n
      Register B: %u%n
      Register C: %u%n
      %n
      Program: %u&,%n|]

    -- Run with given input register A
    putStrLn (intercalate "," (map show (run (Machine a1 b c) program)))
    
    -- Search specific to the kinds of problems we get
    --print (search program b c)

    -- Search using Z3
    res <- optLexicographic \a2 ->
     do minimize "smallest a" a2
        constrain (sRun (SMachine program a2 (fromIntegral b) (fromIntegral c)) program)
    case getModelValue "smallest a" res of
      Just x -> print (x :: Word64)
      Nothing -> putStrLn "no part 2"

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

sRun :: SMachine -> [Int] -> SBool
sRun m0 pgm = go m0 pgm
  where
    go m = \case
      0 : x : ip' -> go m{ sA = sA m `sShiftRight` combo x } ip'
      6 : x : ip' -> go m{ sB = sA m `sShiftRight` combo x } ip'
      7 : x : ip' -> go m{ sC = sA m `sShiftRight` combo x } ip'
      1 : x : ip' -> go m{ sB = sB m `xor`  fromIntegral x } ip'
      2 : x : ip' -> go m{ sB = 7    .&.           combo x } ip'
      4 : _ : ip' -> go m{ sB = sB m `xor`         sC m    } ip'
      3 : x : ip' -> ite (sA m .== 0) (go m{sA=0} ip') (go m (drop x pgm))
      5 : x : ip' ->
        case outs m of
          []   -> sFalse
          o:os -> combo x .&. 7 .== fromIntegral o .&& go m{ outs = os } ip'
      _ -> fromBool (null (outs m))
      where
        combo = \case
          0 -> 0; 1 -> 1; 2 -> 2; 3 -> 3
          4 -> sA m; 5 -> sB m; 6 -> sC m
          _ -> error "invalid combo operand"

search :: [Int] -> Int -> Int -> Int
search pgm b c = head (go =<< [0 .. 7])
  where
    go a =
      case run (Machine a 0 0) pgm of
        out | out == pgm           -> [a]
            | out `isSuffixOf` pgm -> go =<< take 8 [8 * a ..]
            | otherwise            -> []
