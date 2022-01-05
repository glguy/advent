{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/10>

In my original attempt I considered messages with a window size of 20.
Now that I know the message is only 10 high, I've narrowed my predicate.

-}
module Main (main) where

import Advent.Format (format)
import Advent.Coord (coordRow, drawCoords, Coord(..))
import Data.Foldable (asum)
import Data.List (transpose)
import Data.Maybe (fromJust, isJust, isNothing)

-- | Print the answers to day 10
main :: IO ()
main =
 do let toSim (px, py, dx, dy) = iterate (C dy dx +) (C py px)
    input <- map toSim <$> [format|2018 10 (position=< *%d, *%d> velocity=< *%d, *%d>%n)*|]
    putStr $ fromJust $ asum $ zipWith draw [0..] $ transpose input

-- | Given a number of seconds and the current list of particles,
-- render the particles to a string when they are close enough together
-- to be considered interesting to look at.
draw :: Int -> [Coord] -> Maybe String
draw i ps
  | hirow - lorow <= 10 = Just (show i ++ "\n" ++ picture)
  | otherwise           = Nothing
  where
    lorow = minimum (map coordRow ps)
    hirow = maximum (map coordRow ps)
    picture = drawCoords ps
