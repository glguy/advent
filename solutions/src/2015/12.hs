{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2015
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/12>

Sum up the numbers in a JSON value.

Rather than pull in a heavy JSON parsing dependency, this
just parses out the subset of JSON that the problem uses.

>>> :main + "[1,2,3]\n"
6
6

>>> :main + "{\"a\":2,\"b\":4}\n"
6
6

>>> :main + "{\"a\":{\"b\":4},\"c\":-1}\n"
3
3

>>> :main + "[1,{\"c\":\"red\",\"b\":2},3]\n"
6
4

>>> :main + "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}\n"
15
0

>>> :main + "[1,\"red\",5]\n"
6
6

-}
module Main where

import Advent (format)
import Control.Applicative (Alternative((<|>)))
import Text.ParserCombinators.ReadP (ReadP, between, readS_to_P, sepBy)

-- | >>> :main
-- 119433
-- 68466
main :: IO ()
main =
 do value <- [format|2015 12 @p%n|]
    print (numbers       value)
    print (nonredNumbers value)

p :: ReadP Value
p =
  Object <$> between "{" "}" (pEntry `sepBy` ",") <|>
  Array  <$> between "[" "]" (p `sepBy` ",") <|>
  String <$> pString <|>
  Number <$> readS_to_P reads

pString :: ReadP String
pString = readS_to_P reads

pEntry :: ReadP Value
pEntry = pString *> ":" *> p

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
