{-# Language BlockArguments, LambdaCase, QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/13>

Play a game of breakout! This solution uses a greedy strategy of
simply moving the paddle toward the ball and waiting for a win.

-}
module Main (main) where

import Advent (count, format)
import Advent.Coord (Coord(C), drawPicture)
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Intcode (Effect(Input, Halt, Output), Machine, run, new, set)

-- | >>> :main
-- █████████████████████████████████████████████
-- █                                           █
-- █   ◇◇ ◇◇◇◇◇ ◇  ◇ ◇◇◇ ◇◇◇◇ ◇◇◇◇ ◇◇◇ ◇  ◇◇◇  █
-- █ ◇ ◇◇◇◇  ◇◇  ◇◇ ◇◇◇◇ ◇◇◇◇   ◇◇◇ ◇ ◇◇◇◇◇◇◇  █
-- █   ◇◇◇◇   ◇  ◇◇◇◇◇◇◇◇  ◇   ◇◇◇◇◇ ◇◇  ◇◇ ◇◇ █
-- █ ◇◇ ◇◇   ◇ ◇◇ ◇ ◇◇◇◇◇  ◇◇◇ ◇  ◇◇ ◇ ◇ ◇◇◇◇◇ █
-- █ ◇  ◇◇◇  ◇◇  ◇◇◇◇◇◇ ◇◇◇◇◇ ◇◇ ◇  ◇◇◇◇◇◇◇  ◇ █
-- █ ◇ ◇◇◇◇◇◇  ◇ ◇◇◇◇ ◇◇◇◇◇◇◇ ◇◇◇ ◇◇◇ ◇ ◇◇◇ ◇◇ █
-- █ ◇◇◇◇◇◇ ◇◇◇◇  ◇ ◇ ◇  ◇◇ ◇◇◇◇◇◇◇◇◇◇◇◇◇◇◇◇◇◇ █
-- █ ◇ ◇◇◇◇◇◇ ◇◇ ◇◇◇◇◇◇◇   ◇◇  ◇◇◇◇ ◇ ◇◇◇◇◇◇◇◇ █
-- █ ◇◇◇◇◇◇◇◇◇◇◇ ◇◇◇◇◇◇◇ ◇ ◇ ◇ ◇◇◇◇  ◇◇◇◇ ◇◇◇◇ █
-- █ ◇◇ ◇◇  ◇◇◇◇◇◇◇◇◇ ◇◇ ◇ ◇◇ ◇◇◇◇◇◇  ◇◇◇◇◇ ◇  █
-- █  ◇◇ ◇ ◇◇◇◇◇   ◇◇◇  ◇◇◇◇◇◇ ◇◇◇ ◇◇  ◇◇◇◇ ◇  █
-- █  ◇◇◇ ◇◇ ◇◇◇ ◇◇◇◇  ◇◇◇◇◇◇◇◇◇ ◇◇ ◇◇ ◇◇◇ ◇◇◇ █
-- █ ◇◇◇ ◇◇◇◇◇◇◇ ◇ ◇ ◇◇◇◇◇◇◇    ◇◇◇◇  ◇◇◇◇◇◇◇◇ █
-- █ ◇◇    ◇◇◇◇◇ ◇◇◇   ◇◇ ◇◇   ◇◇◇ ◇◇◇◇◇◇◇◇◇◇◇ █
-- █  ◇◇◇  ◇◇ ◇ ◇◇◇  ◇◇ ◇   ◇  ◇ ◇◇◇  ◇◇ ◇◇◇◇◇ █
-- █  ◇◇◇  ◇◇   ◇ ◇◇ ◇ ◇◇◇◇◇◇◇◇◇◇ ◇◇ ◇◇◇ ◇◇◇◇◇ █
-- █                                           █
-- █                   ✪                       █
-- █                                           █
-- █                                           █
-- █                     ―                     █
-- █                                           █
-- 462
-- 23981
main :: IO ()
main =
 do mach <- new <$> [format|2019 13 %d&,%n|]
    let picture1 = part1 (run mach)
    putStr (drawPicture (fmap paint picture1))
    print (count 2 picture1)
    print (part2 Nothing Nothing 0 (run (set 0 2 mach)))

-- | Count the number of screen locations assigned to @2@.
part1 :: Effect -> Map Coord Int
part1 = go Map.empty
  where
    go blocks = \case
      Halt -> blocks
      Output x (Output y (Output t e)) ->
        go (Map.insert (C y x) t blocks) e
      _ -> error "part1: bad program"

-- | Play the breakout game to completion and report the final score.
part2 ::
  Maybe Int {- ^ location of ball -} ->
  Maybe Int {- ^ location of paddle -} ->
  Int       {- ^ current score -} ->
  Effect    {- ^ program effect -} ->
  Int       {- ^ final score -}
part2 ball paddle score = \case
  Halt -> score

  Output (-1) (Output 0 (Output score' effect')) ->
    part2 ball paddle score' effect'

  Output x (Output _ (Output t effect'))
    | t == 3 -> part2 ball (Just x) score effect'
    | t == 4 -> part2 (Just x) paddle score effect'
    | otherwise -> part2 ball paddle score effect'

  Input f ->
    part2 ball paddle score
      case (ball, paddle) of
        (Just b, Just p) -> f (signum (b-p))
        _ -> f 0

  _ -> error "part2: bad program"

-- | Render tiles as characters for viewing.
paint :: Int -> Char
paint 0 = ' '
paint 1 = '█'
paint 2 = '◇'
paint 3 = '―'
paint 4 = '✪'
paint _ = error "paint: invalid argument"
