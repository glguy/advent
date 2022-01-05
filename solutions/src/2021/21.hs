{-# Language ImportQualifiedPost, QuasiQuotes, GeneralisedNewtypeDeriving #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/21>

Play a game on a simple game of play players on a circular board with
9 spaces. The players roll dice to advance to a numbered space and
earn that many points.

Part 1 simulates the game directly and part 2 takes advantage of
there being no interaction between the two players to simulate
their play separately.

-}
module Main (main) where

import Advent (counts, format)
import Control.Monad (replicateM)
import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map.Strict qualified as Map

-- | >>> :main
-- 428736
-- 57328067654557
main :: IO ()
main =
 do (p1,p2) <- [format|2021 21 Player 1 starting position: %u%nPlayer 2 starting position: %u%n|]
    print (part1 0 p1 p2 0 0)
    print (part2 p1 p2)

-- | Compute the @die rolls * losing score@ once one player
-- wins with 1000 points.
part1 ::
  Int {- ^ turn counter -} ->
  Int {- ^ player 1 location -} ->
  Int {- ^ player 2 location -} ->
  Int {- ^ player 1 score    -} ->
  Int {- ^ player 2 score    -} ->
  Int {- ^ player 2 score * 3 * turns -}
part1 turns p1 p2 p1s p2s
  | p1s' >= 1000 = 3 * turns' * p2s
  | otherwise    = part1 turns' p2 p1' p2s p1s'
  where
    turns' = turns + 1
    p1'    = wrap (p1 + 6 - turns) 10
    p1s'   = p1s + p1'

-- | Count the number of ways player 1 can win given that the game is played
-- rolling 3d3.
part2 ::
  Int {- ^ player 1's starting location -} ->
  Int {- ^ player 2's starting location -} ->
  Int {- ^ ways player 1 can win -}
part2 p1 p2 = sum (zipWith (*) p1Wins p2Live)
  where
    rolls  = counts (sum <$> replicateM 3 [1..3])
    p1Wins = unfoldr (p2step 21 rolls) (Map.singleton (p1,0) 1)
    p2Wins = unfoldr (p2step 21 rolls) (Map.singleton (p2,0) 1)
    p2Live = scanl (\acc w -> acc * sum rolls - w) 1 p2Wins

-- | Play a turn of the part 2 game.
p2step ::
  Int                    {- ^ target score -} ->
  Map Int Int            {- ^ roll distribution -} ->
  Map.Map (Int, Int) Int {- ^ live games ((location, score), ways) -} ->
  Maybe (Int, Map.Map (Int, Int) Int) {- ^ wins and next turn's live games -}
p2step goal rolls games
  | Map.null games = Nothing
  | otherwise      = Just (sum wins, live)
  where
    (wins, live) =
      Map.partitionWithKey (\(_,score) _ -> score >= goal) $
      Map.fromListWith (+)
        [ ((loc', score + loc'), n1*n2)
          | ((loc, score),n1) <- Map.toList games
          , (roll        ,n2) <- Map.toList rolls
          , let loc' = wrap (loc+roll) 10
          ]

-- * Modular arithmetic

-- | Wrap number between @1@ and an inclusive upper bound
wrap :: Int {- ^ value -} -> Int {- ^ bound -} -> Int
wrap x n = (x - 1) `mod` n + 1
