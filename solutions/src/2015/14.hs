module Main where

import Data.List

data Reindeer = Reindeer
  { speed     :: Int   -- ^ units of distance flown per second
  , stamina   :: Int   -- ^ number of seconds flown before rest
  , breaktime :: Int   -- ^ number of seconds rested before flying
  }

main :: IO ()
main =
  do rs <- loadInput
     let race = map (take 2503 . positions) rs
     print (maximum (map last race))
     print (maximum (scores race))

loadInput :: IO [Reindeer]
loadInput = map parseLine . lines <$> readFile "input14.txt"

parseLine :: String -> Reindeer
parseLine str =
  case words str of
    [_, _, _, n, _, _, m, _, _, _, _, _, _, o, _] ->
       Reindeer { speed     = read n
                , stamina   = read m
                , breaktime = read o
                }
    _ -> error str

-- | Compute the position of each reindeer at each second of the race
positions :: Reindeer -> [Int]
positions r
  = partialSums
  $ cycle
  $ replicate (stamina   r) (speed r) ++ replicate (breaktime r) 0

-- | Given a list of race positions return a list of scores
scores :: [[Int]] -> [Int]
scores = map sum . transpose . map awardPoints . transpose

-- | Map each position to 1 point if it's in the lead or 0 otherwise
awardPoints ::
  [Int] {- ^ positions -} ->
  [Int] {- ^ points    -}
awardPoints posns = [ if p == best then 1 else 0 | p <- posns ]
  where
  best = maximum posns

-- | Partial sums starting with first element.
--
-- > paritalSums [1..5]
-- [1,3,6,10,15]
partialSums :: Num a => [a] -> [a]
partialSums [] = []
partialSums xs = scanl1 (+) xs
