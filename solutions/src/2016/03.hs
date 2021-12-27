{-# Language QuasiQuotes #-}
module Main where

import Advent
import Data.List

main :: IO ()
main =
  do input <- [format|3 (( *%d)*%n)*|]
     print (countBy goodTriangle input)
     print (countBy goodTriangle (rearrange input))

rearrange :: [[a]] -> [[a]]
rearrange = chunks 3 . concat . transpose

goodTriangle :: [Int] -> Bool
goodTriangle xs = x + y > z
  where
    [x,y,z] = sort xs
