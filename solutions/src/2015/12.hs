{-# LANGUAGE ViewPatterns, GeneralizedNewtypeDeriving, OverloadedStrings, LambdaCase #-}
module Main where

import Advent (getInputLines)
import Text.ParserCombinators.ReadP (option, string, char, ReadP, sepBy, munch1, between, readP_to_S )
import Control.Applicative
import Data.Char (isDigit)

main :: IO ()
main =
 do [input] <- getInputLines 12
    let value = parseValue input 
    print (numbers       value)
    print (nonredNumbers value)

parseValue :: String -> Value
parseValue (readP_to_S pValue -> [(x,_)]) = x
parseValue x = error ("bad input: " ++ x)

pValue :: ReadP Value
pValue =
  Object <$> between (char '{') (char '}') (pEntry `sepBy` char ',') <|>
  Array  <$> between (char '[') (char ']') (pValue `sepBy` char ',') <|>
  String <$> pString <|>
  Number <$> pNumber

pNumber :: ReadP Int
pNumber = read <$> ((++) <$> option "" (string "-") <*> munch1 isDigit)

pString :: ReadP String
pString = between (char '"') (char '"') (munch1 ('"'/=))

pEntry :: ReadP Value
pEntry = pString *> char ':' *> pValue

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
