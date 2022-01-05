{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/23>

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
  do events <- startup <$> [format|2019 23 %d&,%n|]
     print (head     [y | SetY  y <- events])
     print (firstDup [y | SendY y <- events])

data Packet = Packet !Int !Int !Int -- ^ destination, x, y

-- | Map of VM identities to current execution state.
type Network = IntMap (Stream Int Packet)

-- | State of network simulation including most recent NAT packet.
data System = System
  { network :: Network         -- ^ VMs indexed by identity
  , nat     :: Maybe (Int,Int) -- ^ last NAT packet registered
  , sendq   :: Queue Packet    -- ^ Sent packet queue
  }

-- | Network events needed to answer part 1 and 2.
data Event
  = SetY  !Int -- ^ Y value sent to address 255
  | SendY !Int -- ^ NAT packet sent after a system stall
  deriving Show

data Stream a b
  = Emit b (Stream a b)
  | Block (a -> Stream a b)

getPacket :: Effect -> Stream Int Packet
getPacket = get (\x -> get (\y -> get (\z -> Emit (Packet x y z) . getPacket)))
  where
    get f e =
      case e of
        Output x m -> f x m
        Input g    -> Block (get f . g)
        _          -> error "bad intcode input"

(<<) :: Stream a b -> a -> Stream a b
Block f  << i = f i
Emit p e << i = Emit p (e << i)

-- | Run a VM gathering packets until it blocks waiting for input.
gather :: Stream a b -> ([b], Stream a b)
gather (Emit p e) = case gather e of (ps, e') -> (p:ps, e')
gather e          = ([], e)

stepNetwork :: (Network -> ([Packet], Network)) -> System -> [Event]
stepNetwork f sys =
  case f (network sys) of
    (ps, net) -> tryToSend sys{ network = net, sendq = Queue.appendList ps (sendq sys) }


-- gather up any packets ready to send at the outset
startup :: [Int] -> [Event]
startup pgm = stepNetwork (traverse gather) sys
  where
    eff = getPacket (run (new pgm))
    sys = System { network = IntMap.fromList [ (i, eff << i) | i <- [0..49]]
                 , sendq   = Queue.Empty
                 , nat     = Nothing }

tryToSend :: System -> [Event]
tryToSend sys =
  case sendq sys of
    p :<| ps    -> deliver p sys{ sendq = ps }
    Queue.Empty -> stalled sys

stalled :: System -> [Event]
stalled sys =
  case nat sys of
    Just (x,y) -> SendY y : deliver (Packet 0 x y) sys
    Nothing    -> stepNetwork (traverse (gather . (<< negate 1))) sys

deliver :: Packet -> System -> [Event]
deliver (Packet dst x y) sys
  | dst == 255 = SetY y : tryToSend sys{ nat = Just (x,y) }
  | otherwise  = stepNetwork (updateF (gather . (<<y) . (<<x)) dst) sys




updateF :: Applicative f => (a -> f a) -> Int -> IntMap a -> f (IntMap a)
updateF = IntMap.alterF . traverse

firstDup :: Eq a => [a] -> a
firstDup ys = head [ a | (a,b) <- zip ys (tail ys), a==b ]
