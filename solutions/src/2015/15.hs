{-# Language QuasiQuotes #-}
module Main where

import Advent.Format (format)
import Data.List (transpose)

main :: IO ()
main =
 do input <- [format|15 (%s: (%s %ld)&(, )%n)*|]
    let stats = map (map snd . snd) input
        n = fromIntegral (length input)
        possibilities = computeStats stats <$> divisions n 100

    print (maximum (map score possibilities))
    print (maximum [score meal | meal <- possibilities, last meal == 500])

score ::
  [Integer] {- ^ properties list, calories are last -} ->
  Integer   {- ^ score for recipe                   -}
score = product . init

computeStats ::
  [[Integer]] {- ^ properties for all ingredients -} ->
  [Integer]   {- ^ divisions                      -} ->
  [Integer]   {- ^ cumulative properties          -}
computeStats props divs
  = map (max 0 . sum)              -- compute sum of each property
  $ transpose                      -- compute lists of each property
  $ zipWith (map . (*)) divs props -- scale up properties by ingredient

divisions ::
  Integer     {- ^ number of divisions -} ->
  Integer     {- ^ amount to divide    -} ->
  [[Integer]] {- ^ all possible divisions -}
divisions 1   n = [[n]]
divisions cnt n =
  do x  <- [1..n-cnt+1]
     xs <- divisions (cnt - 1) (n-x)
     return (x:xs)

everyOther :: [a] -> [a]
everyOther (_:x:xs) = x : everyOther xs
everyOther _        = []
