module Main where

import Data.List

main :: IO ()
main =
  do steps <- iterate lookAndSay <$> loadInput
     print (length (steps !! 40))
     print (length (steps !! 50))

loadInput :: IO String
loadInput = head . words <$> readFile "input10.txt"

lookAndSay :: String -> String
lookAndSay = foldr aux [] . group
  where
  aux xs = shows (length xs)
         . showChar (head xs)
