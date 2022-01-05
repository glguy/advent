{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/22>

-}
module Main (main) where

import Advent
import Advent.Format (format)
import Data.Foldable
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V

-- |
-- >>> :main
-- 35818
-- 34771
main :: IO ()
main =
  do (xs,ys) <- [format|2020 22 Player 1:%n(%u%n)*%nPlayer 2:%n(%u%n)*|]
     let p1 = Seq.fromList xs
     let p2 = Seq.fromList ys
     print (score (play1 p1 p2))
     print (score (snd (play2 Set.empty p1 p2)))

-- | Game deck, draw from left side
type Deck = Seq Int

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList

-- | representation of a game state used to find cycles
type Rep = Vector Int

characterize :: Deck -> Deck -> Rep
characterize xs ys = V.fromList (toList xs ++ (-1) : toList ys)

------------------------------------------------------------------------

-- | Play the game according to part 1 rules and return the winning deck
play1 :: Deck -> Deck -> Deck
play1 Empty xs = xs
play1 xs Empty = xs
play1 (x :<| xs) (y :<| ys)
  | x > y     = play1 (xs :|> x :|> y) ys
  | otherwise = play1 xs (ys :|> y :|> x)

------------------------------------------------------------------------

-- | Play the game according to part 2 rules and report if player 1 won
-- and the winning deck. Takes a set of previously seen game states to
-- eliminate loops.
play2 :: Set Rep -> Deck -> Deck -> (Bool, Deck)
play2 _ Empty xs = (False, xs)
play2 _ xs Empty = (True, xs)
play2 seen xxs@(x :<| xs) yys@(y :<| ys)

  -- P1 wins loops
  | Set.member here seen = (True, xxs)

  -- recursive game
  | x <= Seq.length xs, y <= Seq.length ys
  , let x' = Seq.take x xs
  , let y' = Seq.take y ys
  , let x1 = maximum x' -- best P1 card
  , let y1 = maximum y' -- best P2 card

    -- if P1 has the high card that can't be lost to a
    -- recursive game then he will always eventually win:
    -- He'll never lose that card and wins in the case of
    -- a loop. The highest card is always at least as large
    -- as the number of cards in the game because all cards
    -- are unique and start at 1, therefore it can never
    -- trigger a recursive game.
  = if x1 > y1 || fst (play2 Set.empty x' y')
      then p1win
      else p2win

  -- regular game
  | x > y     = p1win
  | otherwise = p2win

  where
    here  = characterize xxs yys
    seen1 = Set.insert here seen
    p1win = play2 seen1 (xs :|> x :|> y) ys
    p2win = play2 seen1 xs (ys :|> y :|> x)

