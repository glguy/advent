{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/7>

-}
{-# Language OverloadedStrings, DeriveTraversable #-}
module Main (main) where

import Advent        (format, ordNub)
import Data.Char     (ord)
import Data.Foldable (toList)
import Data.List     (delete, find, sort, unfoldr)
import Data.Map      (Map)
import Data.Map qualified as Map

-- | Print the answers to day 7
--
-- >>> :main
-- ACBDESULXKYZIMNTFGWJVPOHRQ
-- 980
main :: IO ()
main =
  do input <- [format|7 (Step %c must be finished before step %c can begin.%n)*|]
     let queue = newWorkQueue [Dep x y | (x,y) <- input]
     putStrLn (part1 queue)
     print (part2 queue)

-- | Given a work queue compute the order that a single elf would complete
-- the tasks.
part1 :: Eq a => WorkQueue a -> [a]
part1 = unfoldr go
  where
    go q = do (x, q') <- nextTask q
              Just (x, finishTasks [x] q')

-- | Compute time required to complete all of the work defined by the given
-- work queue given 5 parallel workers.
part2 :: WorkQueue Char -> Int
part2 = part2' 0 Map.empty

-- | 'part2' worker loop.
part2' ::
  Int            {- ^ time accumulator                      -} ->
  Map Char Int   {- ^ current tasks with time remaining     -} ->
  WorkQueue Char {- ^ task queue                            -} ->
  Int            {- ^ total time spent to complete all work -}
part2' time work queue

  -- Workers available and work ready to start
  | Map.size work < 5
  , Just (next, queue') <- nextTask queue =
      part2' time (Map.insert next (timeRequired next) work) queue'

  -- All work complete
  | Map.null work = time

  -- No work ready to start, make progress on current work
  | otherwise =
    let step          = minimum work
        (done, work') = Map.partition (0==) (subtract step <$> work)
        queue'        = finishTasks (Map.keys done) queue
    in part2' (time+step) (Map.filter (>0) work') queue'


-- | Compute time required for task by name.
timeRequired :: Char -> Int
timeRequired c = ord c - ord 'A' + 61

------------------------------------------------------------------------

-- | Track a dependency between two items
data Dep a =
  -- | 'depBefore' must be finished before 'depAfter'
  Dep { depBefore, depAfter :: a }
  deriving (Read, Show, Functor, Foldable, Traversable)

-- | Track remaining tasks and dependencies between tasks.
data WorkQueue a = WorkQueue [Dep a] [a]

-- | Construct a new work queue given the task dependencies. Task IDs
-- are computed by checking the IDs mentioned in the dependencies.
newWorkQueue :: Ord a => [Dep a] -> WorkQueue a
newWorkQueue deps = WorkQueue deps remaining
  where
    remaining = ordNub (sort (concatMap toList deps))

-- | Find the next task available to be started and remove it from
-- the work queue.
nextTask :: Eq a => WorkQueue a -> Maybe (a, WorkQueue a)
nextTask (WorkQueue deps remaining) =
  do next <- find (`notElem` map depAfter deps) remaining
     Just (next, WorkQueue deps (delete next remaining))

-- | Mark a list of tasks as completed so that tasks depending on them
-- can be started.
finishTasks :: Eq a => [a] -> WorkQueue a -> WorkQueue a
finishTasks tasks (WorkQueue deps remaining) = WorkQueue deps' remaining
  where
    deps' = filter (\x -> depBefore x `notElem` tasks) deps
