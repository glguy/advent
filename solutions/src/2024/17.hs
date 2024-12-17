{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/17>

-}
module Main where

import Advent (format)
import Data.List (intercalate)
import Data.SBV
    ( Word64, optLexicographic, free, minimize, sShiftRight,
      (.==), (./=), shiftR, xor, (.&.),
      SWord64, constrain,
      Symbolic, getModelValue)

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
    
    putStrLn (intercalate "," (map show (run program 0 a b c)))

    res <- optLexicographic
      do a  <- free "a" :: Symbolic SWord64    
         minimize "smallest" a 
         direct a program
    case getModelValue "a" res of
      Just x -> print (x :: Word64)
      Nothing -> fail "no solution"

run :: [Int] -> Int -> Int -> Int -> Int -> [Int]
run pgm ip a b c =
  let combo 0 = 0
      combo 1 = 1
      combo 2 = 2
      combo 3 = 3
      combo 4 = a
      combo 5 = b
      combo 6 = c in
  case drop ip pgm of
    0 : x : _ -> run pgm (ip+2) (a `shiftR` combo x) b c
    1 : x : _ -> run pgm (ip+2) a (b `xor` x) c
    2 : x : _ -> run pgm (ip+2) a (combo x .&. 7) c
    3 : x : _ | a == 0    -> run pgm (ip+2) a b c
              | otherwise -> run pgm x a b c
    4 : _ : _ -> run pgm (ip+2) a (b `xor` c) c
    5 : x : _ -> combo x .&. 7 : run pgm (ip+2) a b c
    6 : x : _ -> run pgm (ip+2) a (a `shiftR` combo x) c
    7 : x : _ -> run pgm (ip+2) a b (a `shiftR` combo x)
    _ -> []

direct :: SWord64 -> [Int] -> Symbolic ()
direct a [] = constrain (a .== 0)
direct a (o:os) =
  do constrain (a ./= 0)
     b <- pure (a .&. 7)
     b <- pure (b `xor` 2)
     c <- pure (a `sShiftRight` b)
     b <- pure (b `xor` c)
     b <- pure (b `xor` 3)
     constrain ((b .&. 7) .== fromIntegral o)
     a <- pure (a `shiftR` 3)
     direct a os
