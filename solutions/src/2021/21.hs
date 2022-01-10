{-# Language ImportQualifiedPost, QuasiQuotes, MonadComprehensions, GeneralisedNewtypeDeriving #-}
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
import Control.Applicative (Alternative)
import Control.Monad (replicateM)
import Control.Monad.Trans.Writer.Strict (WriterT(..))
import Data.Coerce (coerce)
import Data.List (scanl', unfoldr)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Product(..))

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
part1 = go 0 0 0
  where
    go turns s1 s2 p1 p2
      | s1' >= 1000 = 3 * turns' * s2
      | otherwise   = go turns' s2 s1' p2 p1'
      where
        turns' = turns + 1
        p1'    = wrap (p1 + 6 - turns) 10
        s1'    = s1 + p1'

-- * Part 2

-- | Count the maximum number of ways a player can win in the most given
-- that the game is played rolling 3d3.
--
-- >>> part2 4 8
-- 444356092776315
part2 ::
  Int {- ^ player 1's starting location -} ->
  Int {- ^ player 2's starting location -} ->
  Int {- ^ ways player 1 can win -}
part2 p1 p2 = max u1 u2
  where
    w1 = wins p1                        -- ways player 1 wins by turn
    w2 = wins p2                        -- ways player 2 wins by turn
    l1 = toLoses w1                     -- ways player 1 hasn't won by turn
    l2 = toLoses w2                     -- ways player 2 hasn't won by turn
    u1 = sum (zipWith (*) w1 l2)        -- universes in which player 1 wins
    u2 = sum (zipWith (*) w2 (tail l1)) -- universes in which player 2 wins

-- | Compute the ways a player can win in part 2 per turn given a starting position.
--
-- >>> wins 4
-- [0,0,4608,249542,3219454,24905476,77993473,62172638,8678745,53217]
wins :: Int -> [Int]
wins x = unfoldr p2step (Map.singleton (x, 0) 1)

-- | Compute the ways a player can not win in part 2 per turn given a starting position.
--
-- >>> toLoses (wins 8)
-- [1,27,729,17953,254050,1411009,3520415,2121762,219716,1206,0]
toLoses :: [Int] -> [Int]
toLoses = scanl' (\acc w -> acc * sum rolls - w) 1

-- | Advance the counts of states by playing one turn of the game.
p2step ::
  Map (Int, Int) Int {- ^ live games ((location, score), ways) -} ->
  Maybe (Int, Map (Int, Int) Int) {- ^ wins and next turn's live games -}
p2step games
  | Map.null games = Nothing
  | otherwise      = Just (sum winStates, games')
  where
    (winStates, games') =
      Map.partitionWithKey (\(_,score) _ -> score >= 21) $
      fromCounter
        [ (loc', score + loc')
          | (loc, score) <- toCounter games
          , roll         <- toCounter rolls
          , let loc' = wrap (loc+roll) 10
          ]

-- | Outcomes of rolling 3d3
--
-- >>> rolls
-- fromList [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]
rolls :: Map Int Int
rolls  = counts (sum <$> replicateM 3 [1..3])

-- * Modular arithmetic

-- | Wrap number between @1@ and an inclusive upper bound.
--
-- >>> [wrap i 4 | i <- [-2..6]]
-- [2,3,4,1,2,3,4,1,2]
wrap :: Int {- ^ value -} -> Int {- ^ bound -} -> Int
wrap x n = (x - 1) `mod` n + 1

-- * Tracking counts

-- | Type for backtracking computations that can keep track of how many
-- ways a state is reachable. This allows us to alternate between two
-- useful representations:
--
-- * @[(a,Int)]@ - good for backtracking
-- * @Map a Int@ - good for consolidation
newtype Counter a = Counter (WriterT (Product Int) [] a)
  deriving (Functor, Applicative, Monad, Alternative)

-- | Creates a 'Counter' computation that represents all the keys of a 'Map'
-- occurring as many times as indicated by that key's value.
--
-- @
-- toCounter (Map.singleton k 1) === pure k
-- @
toCounter :: Map a Int -> Counter a
toCounter = coerce . Map.assocs

-- | Run a 'Counter' accumulating all the path counts into a single 'Map'
fromCounter :: Ord a => Counter a -> Map a Int
fromCounter = Map.fromListWith (+) . coerce
