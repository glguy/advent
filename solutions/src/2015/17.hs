{-# Language QuasiQuotes #-}
module Main where

import Advent (counts, format)

main :: IO ()
main =
  do input <- [format|17 (%u%n)*|]
     let combos = combinations 0 input 150 []
     print (length combos)
     print (foldr const undefined (counts combos))

-- | Given a list of container sizes and an amount,
-- return a list of the ways to chose a subset of those containers
-- so that they sum to the desired amount. The resulting list
-- is arranged by number of containers used. The nth element uses
-- n-containers (zero-indexed).
combinations :: Int -> [Int] -> Int -> [Int] -> [Int]
combinations used _ 0 = (used:)
combinations _ [] _ = id
combinations used (x:xs) amt =
  (if x <= amt then combinations (used+1) xs (amt-x) else id)
  . combinations used xs amt
  