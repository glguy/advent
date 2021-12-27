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
import Data.Map (Map)
import Data.Map.Strict qualified as SMap
import System.Environment (getArgs)
import Text.Printf (printf)

-- | Get the input for the given day.
--
-- If a filename is provided in the command line that will be used as the
-- input file.
--
-- If the filename is @-@ the stdin will be used as the input file.
--
-- Otherwise the input text file corresponding to the day number will be used.
getRawInput :: Int {- ^ day number -} -> IO String
getRawInput i =
  do args <- getArgs
     case args of
       []    -> readFile (printf "inputs/input%02d.txt" i)
       "-":_ -> getContents
       fn:_  -> readFile fn

-- | Default input filename given a day number
inputFileName :: Int {- ^ day -} -> FilePath
inputFileName = printf "inputs/input%02d.txt"

-- | Load input file as a list of lines.
getInputLines :: Int -> IO [String]
getInputLines i = lines <$> getRawInput i

-- | Load input file as a rectangular array of characters.
getInputArray :: Int -> IO (A.UArray Coord Char)
getInputArray i =
  do xs <- getInputLines i
     pure $! A.listArray (C 0 0, C (length xs - 1) (length (head xs) - 1)) (concat xs)

-- | Load input file as a 2-dimensional map of characters.
getInputMap :: Int -> IO (Map Coord Char)
getInputMap i =
  do xs <- getInputLines i
     pure $! SMap.fromList (coordLines xs)
