{-# Language QuasiQuotes, TemplateHaskell, BangPatterns, LambdaCase, ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/20>

This problem requires you to hack around inside your input file,
so if this solution doesn't work on yours, you didn't get lucky
and get a easier case like I did, but I assume we all got the same
kind of dumb easy case as the last LCM problem this year.

-}
module Main (main) where

import Advent (format, stageTH)
import Advent.Queue as Queue
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Kind sigil
data K = K | K_PERCENT | K_AMPERSAND
  deriving (Eq, Ord, Show)

data Node
  = Broadcast                      [String] -- ^ broadcast node
  | FlipFlop !Bool                 [String] -- ^ flip-flop
  | Conjunction !Int !(Set String) [String] -- ^ conjunction gate

data Stream a = a :| Stream a

stageTH

-- | Parse the input and print both parts.
--
-- >>> :main
-- 825167435
-- 225514321828633
main :: IO ()
main =
 do input <- [format|2023 20 (@K%a+ -> %a+&(, )%n)*|]
    let incoming = Map.fromListWith (++) [(k, [v]) | (_, v, ks) <- input, k <- ks]
    let nodes = Map.fromList [(name, node incoming name kind conns) | (kind, name, conns) <- input]

    print (part1 0 0 0 (sim nodes))
    print (part2 incoming (sim nodes))

node :: Map String [String] -> String -> K -> [String] -> Node
node incoming name = \case
  K           -> Broadcast
  K_AMPERSAND -> Conjunction (length (incoming Map.! name)) Set.empty
  K_PERCENT   -> FlipFlop False

part1 :: Int -> Int -> Int -> Stream (String, a, Bool) -> Int
part1 n l h ((src,_,sig) :| xs)
  | n == 1000, src == "button" = l * h
  | otherwise =
    part1 (if src == "button" then n+1 else n)
          (if sig then l else l+1)
          (if sig then h+1 else h) xs

part2 :: Map String [String] -> Stream (String, String, Bool) -> Int
part2 incoming msgs = foldl1 lcm [buttonsFor 0 gate msgs | gate <- incoming Map.! specialConj]
  where
    [specialConj] = incoming Map.! "rx"

    buttonsFor n gate ((src, dst, msg) :| xs)
      | "button" == src = buttonsFor (n+1) gate xs
      | msg, src == gate, dst == specialConj = n
    buttonsFor n gate (_ :| xs) = buttonsFor n gate xs

sim :: Map String Node -> Stream (String, String, Bool)
sim fwd = go fwd Queue.Empty
  where
    go st (x Queue.:<| q') = dispatch st x q'
    go st q = dispatch st ("button", "broadcaster", False) q
    
    dispatch st (src, dst, msg) q' =
      (src, dst, msg) :|
      case Map.lookup dst st of
        Just (Broadcast next) -> continue st msg next -- forward message
        Just (FlipFlop mode next)
          | not msg -> continue st' out next -- was on sends low
          where
            st' = Map.insert dst (FlipFlop out next) st
            out = not mode
        Just (Conjunction sz inc next) -> continue st' out next
          where
            inc' = mark src msg inc
            st' = Map.insert dst (Conjunction sz inc' next) st
            out = sz /= length inc'
        _ -> go st q' -- ignored
      where
        continue st' msg' next = go st' (Queue.appendList q' [(dst, t, msg') | t <- next])

mark :: Ord a => a -> Bool -> Set a -> Set a
mark key True = Set.insert key
mark key False = Set.delete key
