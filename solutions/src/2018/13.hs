{-# LANGUAGE ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/13>

-}
module Main (main) where

import Data.Array.Unboxed qualified as A
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)

import Advent.Coord
import Advent.Input (getInputArray)

-- | Turns determine the behavior at an intersection
data Turn = NextL | NextR | NextS deriving Show

-- | Cart state includes the current direction of travel as well
-- as the next turn when an intersection is reached.
data Cart = Cart !Velocity !Turn deriving Show

-- | Velocities are stored row then column to match coordinates
type Velocity = Coord

-- | Road is a random-accessible representation of the track.
newtype Road = Road (A.UArray Coord Char)

-- | Carts are stored in a where they will naturally be ordered
-- in the way that the simulation calls for.
type CartQueue = Map Coord Cart


-- | Print the answers to day 13
--
-- >>> :main
-- 50,54
-- 50,100
main :: IO ()
main =
 do road <- Road <$> getInputArray 2018 13
    let carts = findCarts road
    putStrLn (part1 road carts)
    putStrLn (part2 road carts)

-- | Format a coordinate into X,Y notation.
--
-- >>> format (C 10 20)
-- "20,10"
format :: Coord -> String
format (C y x) = show x ++ "," ++ show y

-- | Run the simulation and report the location of the first collision.
part1 :: Road -> CartQueue -> String
part1 road carts = format (simulate (\pos _ _ -> pos) road carts)

-- | Run the simulation and report the position of the final car.
part2 :: Road -> CartQueue -> String
part2 road carts = format (simulate onCollision road carts)
  where
    -- when a car collides, clear that location and resume the simulation
    onCollision pos ready done =
      tick onCollision road (Map.delete pos ready) (Map.delete pos done)

-- | Look up the road element at a particular coordinate
indexRoad :: Road -> Coord -> Char
indexRoad (Road v) c = v A.! c

-- | Find all the initial locations and velocities of the carts.
findCarts :: Road -> CartQueue
findCarts (Road rs) =
  Map.fromList
    [ (pos, Cart vel NextL)
    | (pos, c) <- A.assocs rs
    , vel <- maybeToList (charToVec c)
    ]

-- | Run the simulation to completion. Take the collision behavior
-- as a parameter to allow part1 and part2 to share the same
-- simulation. When a cart collides with another control of
-- the simulation will switch to the collision parameter.
simulate ::
  (Coord -> CartQueue -> CartQueue -> Coord)
            {- ^ collision behavior: position, ready queue, done queue -} ->
  Road      {- ^ road                                                  -} ->
  CartQueue {- ^ starting cart states                                  -} ->
  Coord     {- ^ final cart position                                   -}
simulate onCollision road carts
  | [pos] <- Map.keys carts = pos
  | otherwise               = tick onCollision road carts Map.empty

-- | Run a single tick of the simulation.
tick ::
  (Coord -> CartQueue -> CartQueue -> Coord)
            {- ^ collision behavior: position, ready queue, done queue -} ->
  Road      {- ^ road                                                  -} ->
  CartQueue {- ^ carts ready to move                                   -} ->
  CartQueue {- ^ carts moved this tick                                 -} ->
  Coord     {- ^ final coordinate answer                               -}
tick onCollision road carts done =
  case Map.minViewWithKey carts of
    Nothing -> simulate onCollision road done
    Just ((pos, cart), carts')
      | collision -> onCollision pos' carts' done
      | otherwise -> tick onCollision road carts' (Map.insert pos' cart' done)
      where
        collision     = Map.member pos' done || Map.member pos' carts'
        (pos', cart') = drive road pos cart

-- | Compute the next state of a cart when it is its turn to move
drive :: Road -> Coord -> Cart -> (Coord, Cart)
drive road pos (Cart vel next) = (pos', cart')
  where
    pos' = pos + vel

    cart' =
      case indexRoad road pos' of
        '\\' -> Cart (invert vel) next
        '/'  -> Cart (invert' vel) next
        '+'  -> Cart (turn next vel) (nextTurn next)
        _    -> Cart vel next

-- | Apply a turn to a velocity.
turn :: Turn -> Velocity -> Velocity
turn NextL = turnLeft
turn NextR = turnRight
turn NextS = id

-- | Advance a turn to the next one in sequence.
nextTurn :: Turn -> Turn
nextTurn NextL = NextS
nextTurn NextS = NextR
nextTurn NextR = NextL
