{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Foldable
import Data.Monoid
import Data.Scientific
import qualified Data.ByteString as B

main :: IO ()
main =
  do input <- loadInput "input12.txt"
     print (sumOfNumbers       input)
     print (sumOfNonredNumbers input)

-- | Sum of all numbers in a JSON value.
sumOfNumbers :: Value -> Scientific
sumOfNumbers = sum . getList . numbers

-- | Sum of all numbers in a JSON value after
-- pruning out portions that fail the 'noRed' test.
sumOfNonredNumbers :: Value -> Scientific
sumOfNonredNumbers = sum . getList . nonredNumbers

-- | Load the input file as a JSON value.
loadInput :: FilePath -> IO Value
loadInput filename =
  do contents <- B.readFile filename
     case decodeStrict' contents of
       Just v  -> return v
       Nothing -> fail "Bad JSON document"

-- | List of all the number values in in JSON value.
numbers :: Value -> DList Scientific
numbers v =
  case v of
    Number n -> singleton n
    Object o -> foldMap numbers o
    Array  a -> foldMap numbers a
    _        -> mempty

-- | List of all the number values in in JSON value
-- excluding objects containing the value @"red"@.
nonredNumbers :: Value -> DList Scientific
nonredNumbers v =
  case v of
    Number n                     -> singleton n
    Object o | "red" `notElem` o -> foldMap nonredNumbers o
    Array  a                     -> foldMap nonredNumbers a
    _                            -> mempty

------------------------------------------------------------------------

-- | A list type that doesn't penalize left-nested appends.
newtype DList a = DList (Endo [a]) deriving (Semigroup, Monoid)

singleton :: a -> DList a
singleton = DList . Endo . (:)

getList :: DList a -> [a]
getList (DList f) = appEndo f []
