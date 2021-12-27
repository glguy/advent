{-# Language TemplateHaskell, QuasiQuotes #-}
module Main where

import Advent (arrIx, format)
import Advent.Coord
import Data.Foldable (foldl')
import Data.Array (Array, (!), listArray)

data D = DL | DR | DU | DD

mempty

main :: IO ()
main =
  do cmds <- [format|2 (@D*%n)*|]
     putStrLn (computeCode keys1 cmds)
     putStrLn (computeCode keys2 cmds)

keys1 :: Array Coord Char
keys1 = listArray (C (-1) (-1), C 1 1)
  "123\
  \456\
  \789"

keys2 :: Array Coord Char
keys2 = listArray (C (-2) (-2), C 2 2)
  "..1..\
  \.234.\
  \56789\
  \.ABC.\
  \..D.."

computeCode :: Array Coord Char -> [[D]] -> String
computeCode ks cmds = map (ks!) (tail (scanl (process ks) origin cmds))

process ::
  Array Coord Char {- ^ key pad           -} ->
  Coord            {- ^ starting position -} ->
  [D]              {- ^ command           -} ->
  Coord            {- ^ stopping position -}
process ks = foldl' aux
  where
    aux pos mov
      | isValid ks pos' = pos'
      | otherwise       = pos
      where
        pos' = step pos mov

isValid :: Array Coord Char -> Coord -> Bool
isValid ks i = maybe False (/= '.') (arrIx ks i)

step :: Coord -> D -> Coord
step c mov =
  case mov of
    DL -> left c
    DR -> right c
    DU -> above c
    DD -> below c
