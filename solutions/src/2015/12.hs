{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/12>

Sum up the numbers in a JSON value.

Rather than pull in a heavy JSON parsing dependency, this
just parses out the subset of JSON that the problem uses.

-}
module Main where

import Advent.Input (getInputLines)
import Advent.ReadS (P(..), char, runP, sepBy, between)
import Control.Applicative (Alternative((<|>)))

-- | >>> :main
-- 119433
-- 68466
main :: IO ()
main =
 do [input] <- getInputLines 2015 12
    let value = runP pValue input 
    print (numbers       value)
    print (nonredNumbers value)

pValue :: P Value
pValue =
  Object <$> between "{" "}" (pEntry `sepBy` ",") <|>
  Array  <$> between "[" "]" (pValue `sepBy` ",") <|>
  String <$> pString <|>
  Number <$> P reads

pString :: P String
pString = P reads

pEntry :: P Value
pEntry = pString *> char ':' *> pValue -- use char as : and - lex together

-- | Sum of all the number values in in JSON value.
numbers :: Value -> Int
numbers v =
  case v of
    Number n -> n
    Object o -> sum (map numbers o)
    Array  a -> sum (map numbers a)
    String _ -> 0

-- | Sum of all the number values in in JSON value
-- excluding objects containing the value @"red"@.
nonredNumbers :: Value -> Int
nonredNumbers v =
  case v of
    Number n -> n
    Object o | String "red" `notElem` o -> sum (map nonredNumbers o)
    Array  a -> sum (map nonredNumbers a)
    _        -> 0

data Value
  = Number !Int
  | Array [Value]
  | Object [Value]
  | String String
  deriving (Eq, Ord, Show)
