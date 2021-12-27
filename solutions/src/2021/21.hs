{-# Language BlockArguments, ImportQualifiedPost, QuasiQuotes, GeneralisedNewtypeDeriving #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/21>

-}
module Main (main) where

import Advent.Format (format)
import Advent.Memo (memo4)
import Control.Applicative (Alternative((<|>)))
import Control.Monad (replicateM)
import Control.Monad.Trans.Writer.CPS (runWriterT, writerT, WriterT)
import Data.Coerce (coerce)
import Data.Map.Strict qualified as Map
import Data.Monoid (Product(Product))

-- | >>> :main
-- 428736
-- 57328067654557
main :: IO ()
main =
 do (p1,p2) <- [format|21 Player 1 starting position: %u%nPlayer 2 starting position: %u%n|]
    print (part1 0 p1 p2 0 0)
    print (maximum (snd <$> runPaths (part2 p1 p2 0 0)))

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

-- | Compute the possible ways a the players can win while playing with
-- a 3-sided dice given some starting conditions.
part2 ::
  Int   {- ^ player 1 location -} ->
  Int   {- ^ player 2 location -} ->
  Int   {- ^ player 1 score    -} ->
  Int   {- ^ player 2 score    -} ->
  Paths Bool {- ^ player 1 won -}
part2 = memo4 \p1 p2 p1s p2s ->
  gather
   do move <- threeRolls 
      let p1' = wrap (p1 + move) 10
      let p1s' = p1s + p1'
      if p1s' >= 21
        then pure True
        else not <$> part2 p2 p1' p2s p1s'

-- | Sum of 3d3.
threeRolls :: Paths Int
threeRolls = gather (sum <$> replicateM 3 (pure 1 <|> pure 2 <|> pure 3))

-- * Counting Nondeterminism Computations

-- | Nondeterministic computation that can consolidate
-- paths returning the same value.
newtype Paths a = Paths (WriterT (Product Int) [] a)
  deriving (Functor, Applicative, Monad, Alternative)

-- | Return all values and counts from all the paths.
runPaths :: Paths a -> [(a, Int)]
runPaths (Paths m) = coerce (runWriterT m)

-- | Combine the counts of equal outputs to reduce braching factor.
gather :: Ord a => Paths a -> Paths a
gather (Paths xs) =
  Paths (writerT (Map.toList (Map.fromListWith (+) (runWriterT xs))))

-- * Modular arithmetic

-- | Wrap number between @1@ and an inclusive upper bound
wrap :: Int {- ^ value -} -> Int {- ^ bound -} -> Int
wrap x n = (x - 1) `mod` n + 1
