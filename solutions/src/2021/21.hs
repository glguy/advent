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
    print (part1 p1 p2)
    print (part2 p1 p2)

-- * Part 1

-- | Compute the @die rolls * losing score@ once one player
-- wins with 1000 points.
--
-- >>> part1 4 8
-- 739785
part1 ::
  Int {- ^ player 1 location -} ->
  Int {- ^ player 2 location -} ->
  Int {- ^ player 2 score * roll count -}
part1 p1 p2 = p1step 0 p1 p2 0 0

-- | Worker for 'part1'
p1step ::
  Int {- ^ turn counter -} ->
  Int {- ^ player 1 location -} ->
  Int {- ^ player 2 location -} ->
  Int {- ^ player 1 score    -} ->
  Int {- ^ player 2 score    -} ->
  Int {- ^ player 2 score * roll count -}
p1step turns p1 p2 p1s p2s
  | p1s' >= 1000 = 3 * turns' * p2s
  | otherwise    = p1step turns' p2 p1' p2s p1s'
  where
    turns' = turns + 1
    p1'    = wrap (p1 + 6 - turns) 10
    p1s'   = p1s + p1'

-- * Part 2

-- | Count the number of ways player 1 can win given that the game is played
-- rolling 3d3.
--
-- >>> part2 4 8
-- 444356092776315
part2 ::
  Int {- ^ player 1's starting location -} ->
  Int {- ^ player 2's starting location -} ->
  Int {- ^ ways player 1 can win -}
part2 p1 p2 = sum (zipWith (*) (wins p1) (loses p2))

-- | Compute the ways a player can win in part 2 per turn given a starting position.
--
-- >>> wins 4
-- [0,0,4608,249542,3219454,24905476,77993473,62172638,8678745,53217]
wins :: Int -> [Int]
wins x = unfoldr p2step (Map.singleton (x,0) 1)

-- | Compute the ways a player can not win in part 2 per turn given a starting position.
--
-- >>> loses 8
-- [1,27,729,17953,254050,1411009,3520415,2121762,219716,1206,0]
loses :: Int -> [Int]
loses =  scanl (\acc w -> acc * sum rolls - w) 1 . wins

-- | Worker for 'part2'
p2step ::
  Map.Map (Int, Int) Int {- ^ live games ((location, score), ways) -} ->
  Maybe (Int, Map.Map (Int, Int) Int) {- ^ wins and next turn's live games -}
p2step games
  | Map.null games = Nothing
  | otherwise      = Just (sum wins, live)
  where
    (wins, live) =
      Map.partitionWithKey (\(_,score) _ -> score >= 21) $
      Map.fromListWith (+)
        [ ((loc', score + loc'), n1*n2)
          | ((loc, score),n1) <- Map.toList games
          , (roll        ,n2) <- Map.toList rolls
          , let loc' = wrap (loc+roll) 10
          ]

-- | Outcomes of rolling 3d3
--
-- >>> rolls
-- fromList [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]
rolls :: Map Int Int
rolls  = counts (sum <$> replicateM 3 [1..3])

-- * Modular arithmetic

-- | Wrap number between @1@ and an inclusive upper bound
wrap :: Int {- ^ value -} -> Int {- ^ bound -} -> Int
wrap x n = (x - 1) `mod` n + 1
