module Main where

import Data.Array
import Data.List
import Data.Maybe

main :: IO ()
main =
  do input <- loadInput
     let combos = combinations input 150
     print (sum combos)
     print (fromMaybe 0 (find (/=0) combos))

loadInput :: IO [Int]
loadInput = map read . words <$> readFile "input17.txt"

-- | Given a list of container sizes and an amount,
-- return a list of the ways to chose a subset of those containers
-- so that they sum to the desired amount. The resulting list
-- is arranged by number of containers used. The nth element uses
-- n-containers (zero-indexed).
combinations :: [Int] -> Int -> [Int]
combinations sizes amount = [ t ! (amount, n, i) | i <- [0..n] ]
  where
  n         = length sizes
  sizeArray = listArray (1,n) sizes

  bnds = ( (0,0,0) , (amount, n, n) )
  t    = array bnds [ (i, ways i) | i <- range bnds ]

  ways :: (Int,Int,Int) -> Int
  ways (amt, sizeIx, containers)

    -- Success, you can fit no eggnog into no containers!
    | amt == 0 && containers == 0 = 1

    -- Failure, ran out of containers or didn't enough enough containers
    | amt == 0 || sizeIx == 0 || containers == 0 = 0

    -- This container doesn't fit, try the next one
    | amt < containerSize = t ! (amt, sizeIx - 1, containers)

    -- This container works, let's try with it and without it
    | otherwise = t ! (amt                , sizeIx - 1, containers    )
                + t ! (amt - containerSize, sizeIx - 1, containers - 1)
    where
    containerSize = sizeArray ! sizeIx
