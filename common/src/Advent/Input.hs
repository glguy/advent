{-# Language ImportQualifiedPost, OverloadedStrings #-}
{-|
Module      : Advent.Input
Description : Input file helpers
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

This module provides input files in some of the most commonly
needed formats. It either automatically loads from an @inputs@
directory, or takes the input file as a command-line argument.

-}
module Advent.Input where

import Advent.Coord (Coord(..), coordLines)
import Data.Array.Unboxed qualified as A
import Data.List (findIndex)
import Data.Map (Map)
import Data.Map.Strict qualified as SMap
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- | Get the input for the given day.
--
-- If a filename is provided in the command line that will be used as the
-- input file.
--
-- If the filename is @-@ the stdin will be used as the input file.
--
-- Otherwise the input text file corresponding to the day number will be used.
getRawInput :: Int {- ^ year -} -> Int {- ^ day number -} -> IO String
getRawInput y d =
  do args <- getArgs
     case args of
       []    -> readFile (printf "inputs/%d/%02d.txt" y d)
       "-":_ -> hPutStrLn stderr "Ready!" >> getContents
       "+":input:_ -> pure input
       fn:_  -> readFile fn

-- | Load input file as a list of lines.
getInputLines :: Int {- ^ year -} -> Int {- ^ day -} -> IO [String]
getInputLines y d = lines <$> getRawInput y d

-- | Load input file as a rectangular array of characters.
getInputArray :: Int {- ^ year -} -> Int {- ^ day -} -> IO (A.UArray Coord Char)
getInputArray y d =
 do xs <- getInputLines y d
    w <- case xs of
      [] -> fail "getInputArray: empty grid"
      x : xs ->
        case findIndex (\y -> length y /= w) xs of
          Just i  -> fail ("getInputArray: bad length on line " ++ show (i+2))
          Nothing -> pure w
        where w = length x
    pure $! A.listArray (C 0 0, C (length xs - 1) (w - 1)) (concat xs)

-- | Load input file as a 2-dimensional map of characters.
getInputMap :: Int {- ^ year -} -> Int {- ^ day -} -> IO (Map Coord Char)
getInputMap y d = SMap.fromList . coordLines <$> getInputLines y d
