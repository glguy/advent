module Main where

import Advent.Input (getInputLines)

main :: IO ()
main =
  do ws <- getInputLines 8
     print (sum (part1 <$> ws))
     print (sum (part2 <$> ws))

part1 :: String -> Int
part1 str = 2 + sum (aux (init (tail str)))
  where
  aux ('\\':'"'    :xs) = 1 : aux xs
  aux ('\\':'\\'   :xs) = 1 : aux xs
  aux ('\\':'x':_:_:xs) = 3 : aux xs
  aux (_           :xs) = aux xs
  aux []                = []

part2 :: String -> Int
part2 str = 2 + length (filter isExpand str)
  where
  isExpand x = x `elem` "\\\""
