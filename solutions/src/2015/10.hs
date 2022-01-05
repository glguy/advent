module Main where

import Advent.Input (getInputLines)
import Data.List (group)

main :: IO ()
main =
  do [start] <- getInputLines 2015 10
     let steps = iterate lookAndSay start
     print (length (steps !! 40))
     print (length (steps !! 50))

lookAndSay :: String -> String
lookAndSay = foldr aux [] . group
  where
  aux xs = shows (length xs)
         . showChar (head xs)
