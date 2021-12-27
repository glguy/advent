{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/4>

-}
module Main (main) where

import Advent (counts, getInputLines)
import Data.List (maximumBy, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Time (LocalTime, readSTime, defaultTimeLocale, todMin, localTimeOfDay)

-- | Print solutions to part 1 and part 2 of day 4
--
-- >>> :main
-- 94040
-- 39940
main :: IO ()
main =
  do input <- toSleepMinutes . parseFile <$> getInputLines 4
     print (part1 input)
     print (part2 input)

-- | Log entry actions
data Action
  = Start Guard -- ^ Guard begins shift
  | Wake        -- ^ Current guard wakes up
  | Sleep       -- ^ Current guard falls asleep
  deriving (Show, Read, Eq, Ord)

newtype Guard = Guard { guardId :: Int }
  deriving (Show, Read, Eq, Ord)

-- | Parse a file into lines of entries and sort them by timestamp.
parseFile :: [String] -> [(LocalTime, Action)]
parseFile = sortBy (comparing fst) . map parseLine

-- | Parse one of the log entries as a pair of timestamp and action.
parseLine :: String -> (LocalTime, Action)
parseLine str =
  case readSTime True defaultTimeLocale "[%Y-%m-%d %H:%M]" str of
    [(time, descr)] -> (time, parseAction descr)
    _               -> error ("parseLine: " ++ str)

-- | Parse the action text of a log entry.
parseAction :: String -> Action
parseAction str =
  case words str of
    ["wakes", "up"]                       -> Wake
    ["falls", "asleep"]                   -> Sleep
    ["Guard", '#':num, "begins", "shift"] -> Start (Guard (read num))
    _                                     -> error ("parseAction: " ++ str)

-- | Generate a list of Guard ID and minute pairs for each minute that
-- a particular guard is sleeping.
toSleepMinutes :: [(LocalTime, Action)] -> [(Guard, Int)]
toSleepMinutes = expandMinutes . go (error "no start")
  where
    -- Transform labeled sleep spans into labeled sleep minutes
    expandMinutes xs =
      [ (who, i) | (who, t1, t2) <- xs
                 , i <- [getMinute t1 .. getMinute t2 - 1]]

    -- Transform start, sleep, wake entries into labeled sleep spans
    go _ ((_, Start who) : xs)           = go who xs
    go who ((t, Sleep) : (u, Wake) : xs) = (who, t, u) : go who xs
    go _ []                              = []
    go _ xs                              = error ("toSleepMinutes: " ++ show xs)

-- | Extract the minute from a local time.
getMinute :: LocalTime -> Int
getMinute = todMin . localTimeOfDay

-- | Given a list of guard/minute pairs, find the product of the number
-- of the sleepiest guard multiplied by the minute that guard is sleepiest.
part1 :: [(Guard, Int)] -> Int
part1 sleepMins = guardId sleepyWho * sleepyMin
  where
    sleepyWho = bigKey (counts [n | (n, _) <- sleepMins])
    sleepyMin = bigKey (counts [m | (n, m) <- sleepMins, n == sleepyWho])

-- | Give a list of guard/minute pairs, find the product of the
-- guard that sleeps the most in a particular minute and that minute.
part2 :: [(Guard, Int)] -> Int
part2 sleepMins = guardId who * minute
  where
    (who, minute) = bigKey (counts sleepMins)

-- | Find the key associated with the largest value in a 'Map'.
bigKey :: Ord a => Map k a -> k
bigKey = fst . maximumBy (comparing snd) . Map.toList
