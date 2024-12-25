{-# Language QuasiQuotes, ImportQualifiedPost, TemplateHaskell, BlockArguments, ScopedTypeVariables, DataKinds #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/24>

-}
module Main (main) where

import Advent (stageTH, format, fromDigits)
import Data.List (intercalate, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.SBV
import Text.Printf (printf)

data G = GAND | GOR | GXOR deriving Show

stageTH

-- | >>> :main
main :: IO ()
main =
 do (initialValues, gates) <- [format|2024 24 (%s: %u%n)*%n(%s @G %s -> %s%n)*|]
    let gatemap = Map.fromList [(z, (g, x, y)) | (x,g,y,z) <- gates]
    let vals = Map.fromList initialValues
            <> fmap (\(g,x,y) -> gate g (vals Map.! x) (vals Map.! y)) gatemap
    print (fromDigits 2 (reverse [v | ('z':_, v) <- Map.assocs vals]))
    
    let swaps = [("bjm", "z07"), ("hsw", "z13"), ("z18", "skf"), ("wkr", "nvr")]
        gatemap2 = foldr (uncurry swap) gatemap swaps

    print =<< prove \(x :: SWord 45) (y :: SWord 45) ->
     do let sWires :: Map String SBool
            sWires = Map.fromList
                      ([(printf "x%02d" i, sTestBit x i) | i <- [0..44]]
                    ++ [(printf "y%02d" i, sTestBit y i) | i <- [0..44]]) <>
                      fmap (\(g, x, y) ->
                        case g of
                          GAND -> (sWires Map.! x) .&& (sWires Map.! y)
                          GOR  -> (sWires Map.! x) .|| (sWires Map.! y)
                          GXOR -> (sWires Map.! x) .<+> (sWires Map.! y)

                          ) gatemap2
            outs :: SWord 46
            outs = fromBitsLE [v | ('z':_, v) <- Map.assocs sWires]
        pure (outs .== (sFromIntegral x + sFromIntegral y)) :: Symbolic SBool
  
    putStrLn (intercalate "," (sort [z | (a,b) <- swaps, z <- [a,b]]))

gate :: Bits a => G -> a -> a -> a
gate GOR = (.|.)
gate GXOR = xor
gate GAND = (.&.)

swap :: Ord k => k -> k -> Map k a -> Map k a
swap a b m = Map.insert a (m Map.! b) (Map.insert b (m Map.! a) m)
