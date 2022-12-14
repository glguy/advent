{-# Language QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
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

import Advent (format, stageTH)
import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import Linear (V2(V2))
import Text.ParserCombinators.ReadP (ReadP, get, between, sepBy)

-- | Parse the group string format as defined in Day 9. Parse
-- result is a vector containing the group score and garbage
-- character count.
parseGroup ::
  Int            {- ^ group depth                -} ->
  ReadP (V2 Int) {- ^ group score, garbage count -}
parseGroup n =
  foldl (+) (V2 n 0) <$>
  between "{" "}"
    (sepBy (parseGroup (n+1)  <|>  V2 0 <$> parseGarbage) ",")

-- | Parse a angle-bracket bracketed region of characters and return the
-- number of non-ignored, contained characters. Characters including and
-- following a @!@ are ignored inside garbgae.
parseGarbage :: ReadP Int {- ^ garbage count -}
parseGarbage = "<" *> elt 0

elt :: Int -> ReadP Int
elt n =
 do x <- get
    case x of
      '!' -> get *> elt n
      '>' -> pure n
      _   -> elt (n+1)

-- | Starting parser named with a single letter to work with format.
p :: ReadP (V2 Int)
p = parseGroup 1

stageTH

-- | Print solution for Day 9. Puzzle input can be overriden by command-line
-- argument.
--
-- >>> :main
-- 14212
-- 6569
main :: IO ()
main =
 do answers <- [format|2017 9 @p%n|]
    traverse_ print answers
