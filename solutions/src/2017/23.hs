{-# Language QuasiQuotes, TemplateHaskell, RankNTypes #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/23>

Part 1 is just a copy/paste of day 18

Part 2 was done with manual analysis

-}
module Main where

import Advent        (format)
import Control.Lens  (view, at, non, set, over, Lens')
import Data.Map      (Map)
import Text.Read     (readMaybe)
import qualified Data.Map as Map
import qualified Data.Vector as V

data C = Cset | Cjnz | Cmul | Csub
  deriving Show

mempty

main :: IO ()
main =
  do input <- V.fromList <$> [format|23 (@C (%a|%ld) (%a|%ld)%n)*|]
     let pgm n = runProgram input
     print (pgm 0)

-- | Either lookup a register or return the value of a constant.
(!) ::
  Map Char Integer {- ^ registers          -} ->
  Either Char Integer {- ^ number or register -} ->
  Integer            {- ^ argument value     -}
m ! k =
  case k of
    Right n -> n
    Left v  -> view (reg v) m

runProgram ::
  V.Vector (C, Either Char Integer, Either Char Integer) {- ^ instructions -} ->
  Int               {- ^ multiplies   -}
runProgram cmds = step 0 0 Map.empty
  where
    step acc pc regs =
      case cmds V.!? pc of
        Nothing          -> acc
        Just (Cset,Left x,y) -> step acc (pc+1) (set  (reg x) (regs!y)  regs)
        Just (Csub,Left x,y) -> step acc (pc+1) (over (reg x) (subtract (regs!y)) regs)
        Just (Cmul,Left x,y) -> step (1+acc) (pc+1) (over (reg x) (* (regs!y)) regs)
        Just (Cjnz,x,y) -> step acc (pc+o) regs
          where
            o | regs!x /= 0 = fromIntegral (regs!y)
              | otherwise  = 1

reg :: Char -> Lens' (Map Char Integer) Integer
reg r = at r . non 0
