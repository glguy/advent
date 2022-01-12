{-# Language ScopedTypeVariables, QuasiQuotes, TemplateHaskell, ImportQualifiedPost, BlockArguments #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/21>

I manually decompiled my input to find the embedded program.

This program generates a stream of 24-bit numbers and tests those
against register zero. This stream of numbers eventually repeats
itself. To find the shortest execution we must find the first
number generated in the cycle. To find the longest execution we must
find the last number generated.

-}
module Main (main) where

import Foreign.C (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(peek))
import Language.C.Inline qualified as C

C.include "<inttypes.h>"
C.include "<stdint.h>"
C.include "<stdlib.h>"

-- | Print the answers to day 21
--
-- @
-- 15615244
-- 12963935
-- @
main :: IO ()
main =
  alloca \(part1 :: Ptr CInt) ->
  alloca \(part2 :: Ptr CInt) ->
   do [C.block| void {
      uint64_t seen[0x1000000 / 64] = {0};
      int started = 0;
      uint32_t r5 = 0;

      for(;;) {
        r5 = 0xed43a1
          + 0x04dc53 * (0xff & r5)
          + 0xd802b9 * (0xff & (r5 >> 8))
          + 0x01016b * (1 | 0xff & (r5 >> 16));
        r5 &= 0xffffff;

        if (!started) {
          started = 1;
          *$(int *part1) = r5;
        }

        {
          const size_t i = r5 / 64;
          const uint64_t j = UINT64_C(1) << (r5 % 64);
          if ((seen[i] & j) == j) return;
          seen[i] |= j;
        }

        *$(int *part2) = r5;
      }
      } |]

      print =<< peek part1
      print =<< peek part2
