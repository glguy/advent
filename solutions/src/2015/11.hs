module Main where

import Advent.Input (getInputLines)
import Data.List (tails)

main :: IO ()
main =
  do [key] <- getInputLines 2015 11
     mapM_ putStrLn (take 2 (solutions key))

-- | Compute the list of valid passwords starting from a given one.
-- Note: This process works on reversed passwords with the rules
-- updated to work on reversed strings. This is to make 'nextPassword'
-- easier to write.
solutions :: String -> [String]
solutions = map reverse . filter isGoodPassword . iterate nextPassword . reverse
          . startOnGood

-- | Check that a string satisfies the descending and duplicate letter rules.
isGoodPassword :: String -> Bool
isGoodPassword p = hasPairs [] 2 p && hasDesc p

-- | Test that a string has at least @count@ non-overlapping double, adjacent
-- letters.
hasPairs :: [Char] {- ^ pairs seen so far -} -> Int {- ^ count -} -> String -> Bool
hasPairs _ 0 _  = True
hasPairs seen n (x:y:z)
  | x == y && x `notElem` seen = hasPairs (x:seen) (n-1) z
  | otherwise = hasPairs seen n (y:z)
hasPairs _ _ _  = False

-- | Test that a string has a 3-length descending sequence.
hasDesc :: String -> Bool
hasDesc = any aux . tails
  where
  aux (x:y:z:_) = x == succ y && y == succ z
  aux _         = False

-- | Test that a character is not in the set of @"iol"@
isGoodLetter :: Char -> Bool
isGoodLetter c = 'i' /= c && 'o' /= c && 'l' /= c

-- | Clean out the starting prohibited letters
startOnGood :: String -> String
startOnGood [] = []
startOnGood (x:xs)
  | isGoodLetter x = x : startOnGood xs
  | otherwise = succ x : map (const 'a') xs

-- | Increment a string from left to right while skipping
-- the prohibited characters.
nextPassword :: String -> String
nextPassword []     = "a"
nextPassword (x:xs) =
  case x of
    'z' -> 'a' : nextPassword xs
    _ | isGoodLetter x' -> x' : xs
      | otherwise       -> nextPassword (x':xs)
  where
  x' = succ x
