{-# Language QuasiQuotes, TemplateHaskell #-}
module Main where

import           Advent
import           Advent.Coord

import           Data.List
import qualified Data.Set as Set

data D = DL | DR

mempty

main :: IO ()
main =
  do cmds <- [format|1 (@D%d)&(, )%n|]
     let path = computePath cmds
     print (part1 path)
     print (part2 path)

-- | Given a list of steps determine the ultimate Manhattan-distance from
-- the starting position.
part1 :: [Coord] -> Int
part1 = manhattan origin . last

part2 :: [Coord] -> Maybe Int
part2 = fmap (manhattan origin) . duplicate

computePath :: [(D,Int)] -> [Coord]
computePath = toCoords origin . toSteps north

-- | Find the first duplicate element in a list
duplicate :: Ord a => [a] -> Maybe a
duplicate = aux Set.empty
  where
    aux _    [] = Nothing
    aux seen (x:xs)
      | Set.member x seen = Just x
      | otherwise         = aux (Set.insert x seen) xs

-- | Compute steps taken by following a list of commands
toSteps ::
  Coord     {- ^ initial direction  -} ->
  [(D,Int)] {- ^ commands           -} ->
  [Coord]   {- ^ list of directions -}
toSteps dir0 cmds = concat (snd (mapAccumL aux dir0 cmds))
  where
    aux dir (lr, steps) =
      let dir' = turn lr dir
      in (dir', replicate steps dir')

toCoords ::
  Coord   {- ^ origin -} ->
  [Coord] {- ^ steps  -} ->
  [Coord] {- ^ path   -}
toCoords = scanl (+)

turn :: D -> Coord -> Coord
turn DL = turnLeft
turn DR = turnRight
