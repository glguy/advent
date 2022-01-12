{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/9>

Day 9 poses a problem of parsing a nested bracket structure.

-}
module Main where

import Advent (getInputLines)
import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import Linear (V2(V2))
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, char, get, satisfy, between, sepBy)

-- | Print solution for Day 9. Puzzle input can be overriden by command-line
-- argument.
main :: IO ()
main =
 do [line] <- getInputLines 2017 9
    let [(x,_)] = readP_to_S (parseGroup 1) line
    traverse_ print x

-- | Parse the group string format as defined in Day 9. Parse
-- result is a vector containing the group score and garbage
-- character count.
parseGroup ::
  Int            {- ^ group depth                -} ->
  ReadP (V2 Int) {- ^ group score, garbage count -}
parseGroup n =
  foldl (+) (V2 n 0) <$>
  between (char '{') (char '}')
    (sepBy (parseGroup (n+1)  <|>  V2 0 <$> parseGarbage) (char ','))

-- | Parse a angle-bracket bracketed region of characters and return the
-- number of non-ignored, contained characters. Characters including and
-- following a @!@ are ignored inside garbgae.
parseGarbage :: ReadP Int {- ^ garbage count -}
parseGarbage = char '<' *> elt 0

elt :: Int -> ReadP Int
elt n =
 do x <- get
    case x of
      '!' -> get *> elt n
      '>' -> pure n
      _   -> elt (n+1)
