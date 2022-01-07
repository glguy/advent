{-# Language QuasiQuotes, TemplateHaskell, RankNTypes #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/23>

Part 1 is just a copy/paste of day 18

Part 2 was done with manual analysis. My program counted the number of
composite numbers in the range @[108100, 108117 .. 125100]@

-}
module Main where

import Advent        (format)
import Data.Map      (Map)
import Text.Read     (readMaybe)
import qualified Data.Map as Map
import qualified Data.Vector as V

data C = Cset | Cjnz | Cmul | Csub
  deriving Show

mempty

-- | >>> :main
-- 6241
main :: IO ()
main =
  do input <- V.fromList <$> [format|2017 23 (@C (%a|%ld) (%a|%ld)%n)*|]
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
    Left v  -> Map.findWithDefault 0 v m

runProgram ::
  V.Vector (C, Either Char Integer, Either Char Integer) {- ^ instructions -} ->
  Int               {- ^ multiplies   -}
runProgram cmds = step 0 0 Map.empty
  where
    step acc pc regs =
      case cmds V.!? pc of
        Nothing          -> acc
        Just (Cset,Left x,y) -> step acc (pc+1) (Map.insert x (regs!y) regs)
        Just (Csub,Left x,y) -> step acc (pc+1) (Map.insert x (regs Map.! x - regs!y) regs)
        Just (Cmul,Left x,y) -> step (1+acc) (pc+1) (Map.insert x (regs Map.! x * regs!y) regs)
        Just (Cjnz,x,y) -> step acc (pc+o) regs
          where
            o | regs!x /= 0 = fromIntegral (regs!y)
              | otherwise  = 1
