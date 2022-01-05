{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/17>

-}
module Main (main) where

import Advent.Format (format)
import Advent.Coord
import Data.Char (ord, chr)
import Data.List (inits, intercalate, tails)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)
import Intcode (effectList, intcodeToList, run, new, set)

-- | >>> main
-- 7280
-- 1045393
main :: IO ()
main =
  do inp <- [format|2019 17 %d&,%n|]
     let ascii = map (chr . fromIntegral) (intcodeToList inp [])
         world = Map.fromList (coordLines (lines ascii))
     print $ sum [ coordRow k * coordCol k
                 | k <- Map.keys world
                 , all (\x -> '#' == at x world) (k : cardinal k) ]

     print (part2 inp world)

at :: Coord -> Map Coord Char -> Char
at = Map.findWithDefault '.'

part2 :: [Int] -> Map Coord Char -> Int
part2 inp world =
     last $ effectList (run (set 0 2 (new inp)))
          $ map (fromIntegral . ord) input
  where
    start   = head [ k | (k,'^') <- Map.toList world ]
    p       = path world start north
    input:_ = compress [] Nothing Nothing Nothing p

data Dir = U | D | L | R
  deriving (Eq, Ord, Show)

path :: Map Coord Char -> Coord -> Coord -> [(Dir,Int)]
path world here dir
  | '#' == at (turnLeft  dir + here) world = walk L turnLeft
  | '#' == at (turnRight dir + here) world = walk R turnRight
  | otherwise                              = []
  where
    walk cmd f = (cmd, n) : path world endPoint dir'
      where
        dir'     = f dir
        steps    = takeWhile (\x -> at x world /= '.')
                 $ iterate (dir' +) here
        n        = length steps - 1
        endPoint = last steps

compress ::
  [String] ->
  Maybe [(Dir,Int)] ->
  Maybe [(Dir,Int)] ->
  Maybe [(Dir,Int)] ->
  [(Dir,Int)] ->
  [String]
compress acc a b c [] = [unlines [ intercalate "," (reverse acc)
                                 , maybe "L" instructions a
                                 , maybe "L" instructions b
                                 , maybe "L" instructions c, "n"] ]
compress acc a b c xs =
  do (ys,zs) <- takeWhile (short . fst) (splits xs)
     id [ z | Just ys == a, z <- compress ("A":acc) a b c zs ] ++
        [ z | Just ys == b, z <- compress ("B":acc) a b c zs ] ++
        [ z | Just ys == c, z <- compress ("C":acc) a b c zs ] ++
        [ z | isNothing a,                           z <- compress ("A":acc) (Just ys) b c zs ] ++
        [ z | isJust    a, isNothing b,              z <- compress ("B":acc) a (Just ys) c zs ] ++
        [ z | isJust    a, isJust    b, isNothing c, z <- compress ("C":acc) a b (Just ys) zs ]

short :: [(Dir,Int)] -> Bool
short xs = length (instructions xs) <= 20

instructions :: [(Dir,Int)] -> String
instructions xs = intercalate "," [ show d ++ "," ++ show n | (d,n) <- xs ]

splits :: [a] -> [([a],[a])]
splits xs = tail (zip (inits xs) (tails xs))
