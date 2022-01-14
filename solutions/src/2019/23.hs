{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/23>

This problem builds a packet network out of 50 concurrently executing
intcode machines. It's implemented by keeping track of the individual
machines, a packet delivery queue, and the most recent NAT packet.
As the system is simulated a list of interesting simulation events is
produced that can be processed to compute the answers to both parts of
the problem.

-}
module Main (main) where

import Advent.Format (format)
import Advent.Queue (Queue((:<|)))
import Advent.Queue qualified as Queue
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Intcode (Effect(..), run, new)

-- | >>> :main
-- 22151
-- 17001
main :: IO ()
main =
 do inp <- [format|2019 23 %d&,%n|]
    let events = startup (run (new inp))
    print (head     [y | SetY  y <- events])
    print (firstDup [y | SendY y <- events])

-- * Machine effect processing

-- | A bundle of destination and payload data sent on the network.
data Packet = Packet !Int !Int !Int -- ^ destination, x, y
  deriving Show

-- | Deliver the list of inputs to a machine expecting them, then collect all
-- emitted packets returning a machine once-again waiting for inputs.
resume :: [Int] -> Effect -> ([Packet], Effect)
resume [] (Output d (Output x (Output y e))) = ([Packet d x y], ()) *> resume [] e
resume [] e = pure e
resume (x:xs) (Input f) = resume xs (f x)
resume _ _ = error "resume: machine out of sync"

-- * Event loop

-- | Network events needed to answer part 1 and 2.
data Event
  = SetY  !Int -- ^ Y value sent to address 255
  | SendY !Int -- ^ NAT packet sent after a system stall
  deriving Show

-- | Start a network of 50 machines given the machine template. Start running
-- by waking all machines with their network IDs. The event stream from running
-- this network is then returned.
startup :: Effect -> [Event]
startup mach =
  enq
    (sequence (IntMap.fromList [(i, resume [i] mach) | i <- [0..49]]))
    Queue.Empty
    Nothing

-- | Simulation loop for a running network.
sim ::
  IntMap Effect    {- ^ machines on the network -} ->
  Queue Packet     {- ^ packet delivery queue   -} ->
  Maybe (Int, Int) {- ^ most recently stored NAT -} ->
  [Event]          {- ^ simulation event stream -}
sim net (Packet 255 x y :<| q) _   = SetY  y : sim net q (Just (x,y))
sim net (Packet d   x y :<| q) nat =           enq (updateF d (resume [x,y]) net) q nat
sim net q nat@(Just (x,y))         = SendY y : enq (updateF 0 (resume [x,y]) net) q nat
sim net q nat                      =           enq (traverse  (resume [ -1]) net) q nat

-- | Helper for 'sim' that enqueues the new packets and returns to 'sim' loop.
enq :: ([Packet], IntMap Effect) -> Queue Packet -> Maybe (Int, Int) -> [Event]
enq (ps, net) q = sim net (Queue.appendList ps q)

-- * Utilities

-- | Traversal for an element in an 'IntMap'.
updateF :: Applicative f => Int -> (a -> f a) -> IntMap a -> f (IntMap a)
updateF i f = IntMap.alterF (traverse f) i

-- | Find the first value that occurs twice in a row in a list.
--
-- >>> firstDup [1,2,3,2,1,2,5,5,3,2,1]
-- 5
firstDup :: Eq a => [a] -> a
firstDup ys = head [a | (a,b) <- zip ys (tail ys), a==b]
