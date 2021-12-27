{-# Language BlockArguments #-}
{-# Options_GHC -Wno-orphans #-}
{-|
Module      : Advent.Bench
Description : Helper for running benchmarks
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Bench (NFData(rnf), benchMain) where

import Control.DeepSeq
import Criterion.Main
import System.Environment
import Data.Array.Unboxed as A

instance NFData (A.UArray i e) where
  rnf x = seq x ()


benchMain ::
  NFData a =>
  IO a ->
  [(String, a -> b)] ->
  IO ()
benchMain getInput impls =
  do args <- getArgs
     let args' = drop 1 (dropWhile ("--" /=) args)
     withArgs args' $
       defaultMain
         [ env getInput \inp ->
           bgroup "Implementations"
           [bench name (whnf impl inp) | (name, impl) <- impls]
         ]
