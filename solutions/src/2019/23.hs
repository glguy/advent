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
    let events = startup (newSystem inp)
    print (head     [y | SetY  y <- events])
    print (firstDup [y | SendY y <- events])

-- * Machine effect processing

-- | A bundle of destination and payload data sent on the network.
data Packet = Packet !Int !Int !Int -- ^ destination, x, y
  deriving Show

-- | Deliver the list of inputs to a machine expecting them, then collect all
-- emitted packets returning a machine once-again waiting for inputs.
resume :: [Int] -> Effect -> ([Packet], Effect)
resume [] (Output d (Output x (Output y (resume [] -> (ps,e))))) = (Packet d x y : ps, e)
resume [] e = ([], e)
resume (x:xs) (Input f) = resume xs (f x)
resume _ _ = error "resume: machine out of sync"

-- * System state and basic operations

-- | State of network simulation
data System = System
  { network :: IntMap Effect   -- ^ Machines indexed by identity
  , nat     :: Maybe (Int,Int) -- ^ last NAT payload registered
  , sendq   :: Queue Packet    -- ^ Sent packet queue
  }
  deriving Show

-- | Construct a new 50-machine system given an input program with no
-- NAT packet and an empty delivery queue.
newSystem :: [Int] -> System
newSystem inp = System
  { network = IntMap.fromList [(i, run (new inp)) | i <- [0..49]]
  , sendq   = Queue.Empty
  , nat     = Nothing
  }

-- | Add the given packets to the back of the packet queue.
enq :: [Packet] -> System -> System
enq ps sys = sys{sendq = Queue.appendList ps (sendq sys)}

-- | Pop the next packet off the send queue.
pop :: System -> Maybe (Packet, System)
pop sys =
  case sendq sys of
    Queue.Empty -> Nothing
    p :<| ps    -> Just (p, sys{sendq = ps})

-- * Event loop

-- $doc
-- The event loop is implemented as a state machine where the states are
-- 'startup', 'idle', 'deliver'.  As the state machine
-- progresses it produces a list of 'Event' values indicating important
-- events that happened in the course of simulating the network.

-- | Network events needed to answer part 1 and 2.
data Event
  = SetY  !Int -- ^ Y value sent to address 255
  | SendY !Int -- ^ NAT packet sent after a system stall
  deriving Show

-- | Tell each machine its identity and start event loop. This is the entry-point
-- into the event loop.
startup :: System -> [Event]
startup = wakeNetwork (IntMap.traverseWithKey (resume . pure))

-- | All the machines are waiting for input. Deliver a packet if one is ready,
-- send a NAT packet, or deliver empty inputs to all machines.
idle :: System -> [Event]
idle sys
  | Just (Packet 255 x y, sys') <- pop sys = SetY  y : idle sys'{ nat = Just (x,y) }
  | Just (Packet d   x y, sys') <- pop sys =           wakeNetwork (updateF d (resume [x,y])) sys'
  | Just (x, y)                 <- nat sys = SendY y : wakeNetwork (updateF 0 (resume [x,y])) sys
  | otherwise                              =           wakeNetwork (traverse  (resume [ -1])) sys

-- | Update the network then return to the idle state. Any updated machines should
-- be left stalled with all packets collected.
wakeNetwork :: (IntMap Effect -> ([Packet], IntMap Effect)) -> System -> [Event]
wakeNetwork f (networkF f -> (ps, sys)) = idle (enq ps sys)

-- * Utilities

-- | Lens for 'network' field of 'System'
networkF :: Functor f => (IntMap Effect -> f (IntMap Effect)) -> System -> f System
networkF f sys = (\net -> sys{network = net}) <$> f (network sys)

-- | Traversal for an element in an 'IntMap'.
updateF :: Applicative f => Int -> (a -> f a) -> IntMap a -> f (IntMap a)
updateF i f = IntMap.alterF (traverse f) i

-- | Find the first value that occurs twice in a row in a list.
--
-- >>> firstDup [1,2,3,2,1,2,5,5,3,2,1]
-- 5
firstDup :: Eq a => [a] -> a
firstDup ys = head [a | (a,b) <- zip ys (tail ys), a==b]
