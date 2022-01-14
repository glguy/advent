{-# Language ViewPatterns, QuasiQuotes, ImportQualifiedPost #-}
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
    print (head     [y | Pkt _ (Packet 255 _ y) <- events])
    print (firstDup [y | Pkt True (Packet _ _ y) <- events])

-- * Machine effect processing

-- | A bundle of destination and payload data sent on the network.
data Packet = Packet !Int !Int !Int -- ^ destination, x, y
  deriving Show

-- | Deliver the inputs to a machine expecting them, then collect all
-- emitted packets returning a machine once-again waiting for inputs.
resume :: Int -> Int -> Effect -> ([Packet], Effect)
resume x y (Input (($x) -> Input (($y) -> e))) = gather e
resume _ _ _ = error "resume: machine out of sync"

-- | Collect all packets the machine is ready to emit returning it to a blocked state.
gather :: Effect -> ([Packet], Effect)
gather (Output d (Output x (Output y (gather -> (ps, e))))) = (Packet d x y : ps, e)
gather e = pure e

-- * Event loop

-- | Network events needed to answer part 1 and 2.
data Event = Pkt Bool Packet  -- ^ Packet handled. Flag set True when sent due to NAT
  deriving Show

-- | Start a network of 50 machines given the machine template. Start running
-- by waking all machines with their network IDs. The event stream from running
-- this network is then returned.
startup :: Effect -> [Event]
startup mach = sim
  (Queue.fromList [Packet i i (-1) | i <- [0..49]])
  (error "no NAT packet")
  (IntMap.fromList [(i, mach) | i <- [0..49]])

-- | Simulation loop for a running network.
sim ::
  Queue Packet  {- ^ packet delivery queue   -} ->
  Packet        {- ^ most recently stored NAT -} ->
  IntMap Effect {- ^ machines on the network -} ->
  [Event]       {- ^ simulation event stream -}
sim (p :<| q) nat net = Pkt False p  : deliver q nat p   net
sim q         nat net = Pkt True nat : deliver q nat nat net

-- | Helper for 'sim' that delivers a packet to the network.
deliver :: Queue Packet -> Packet {- ^ NAT -} -> Packet {- ^ current -} -> IntMap Effect -> [Event]
deliver q nat (Packet d x y) net
  | 255 == d                                 = sim q (Packet 0 x y) net
  | (ps, net') <- updateF d (resume x y) net = sim (Queue.appendList q ps) nat net'

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
