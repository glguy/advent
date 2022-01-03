{-# Language ImportQualifiedPost, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/11>

-}
module Main (main) where

import Control.Lens
import Data.Bits
import Data.List
import Data.Maybe
import Advent.Search (bfsOn)
import Advent.SmallSet (SmallSet)
import Advent.SmallSet qualified as SBS

-- Types ---------------------------------------------------------------

data Floor = Floor !SmallSet !SmallSet -- ^ gen micro
  deriving (Eq, Ord, Show)

data Building = Building
  { _bldgSteps    :: !Int
  , _lowerFloors  :: [Floor]
  , _currentFloor :: {-# UNPACK #-} !Floor
  , _higherFloors :: [Floor]
  }
  deriving Show

makeLenses ''Building

-- Main logic and parameters -------------------------------------------

main :: IO ()
main =
  do print (solutionSteps part1)
     print (solutionSteps part2)

part1 :: Building
part1 = Building 0 [] ( mkFloor [0] [0])
                      [ mkFloor [1..4] []
                      , mkFloor [] [1..4]
                      , mkFloor [] [] ]

part2 :: Building
part2 = Building 0 [] ( mkFloor [0..2] [0..2])
                      [ mkFloor [3..6] []
                      , mkFloor [] [3..6]
                      , mkFloor [] [] ]

solutionSteps :: Building -> Maybe Int
solutionSteps b =
  listToMaybe [ b'^.bldgSteps | b' <- bfsOn mkRep advanceBuilding b
                              , isSolved b' ]

--Floor operations -----------------------------------------------------

mkFloor :: [Int] -> [Int] -> Floor
mkFloor xs ys = Floor (SBS.fromList xs) (SBS.fromList ys)

isEmptyFloor :: Floor -> Bool
isEmptyFloor (Floor x y) = SBS.null x && SBS.null y

isValidFloor :: Floor -> Bool
isValidFloor (Floor gens mics) = SBS.null gens || SBS.null (mics SBS.\\ gens)

floorUnion :: Floor -> Floor -> Floor
floorUnion (Floor x y) (Floor u v) = Floor (SBS.union x u) (SBS.union y v)

floorDifference :: Floor -> Floor -> Floor
floorDifference (Floor x y) (Floor u v) = Floor (x SBS.\\ u) (y SBS.\\ v)

pickFromFloor :: Floor -> [Floor]
pickFromFloor (Floor gs ms) = pair ++ twoGens ++ twoMics ++ oneGen ++ oneMic
  where
    gens = SBS.toList gs
    mics = SBS.toList ms
    twoGens = do xs <- SBS.fromList <$> pick2 gens
                 return $! Floor xs SBS.empty
    twoMics = do xs <- SBS.fromList <$> pick2 mics
                 return $! Floor SBS.empty xs
    pair    = do x <- SBS.singleton <$>
                        take 1 (SBS.toList (SBS.intersection gs ms))
                 return $! Floor x x
    oneGen  = do x <- SBS.singleton <$> gens
                 return $! Floor x SBS.empty
    oneMic  = do x <- SBS.singleton <$> mics
                 return $! Floor SBS.empty x

pick2 :: [a] -> [[a]]
pick2 xs = [ [x,y] | x:ys <- tails xs, y <- ys ]

floorRep :: Floor -> Int
floorRep (Floor gens mics) =
  fromIntegral (SBS.setRep gens `shiftL`  7 .|.  SBS.setRep mics)

-- Building operations -------------------------------------------------

isSolved :: Building -> Bool
isSolved b = null (b^.higherFloors) && all isEmptyFloor (b^.lowerFloors)

advanceBuilding :: Building -> [Building]
advanceBuilding b =
  [ b3 & bldgSteps +~ 1
       | subset <- pickFromFloor (b^.currentFloor)
       , b1     <- updateCurrentFloor (`floorDifference` subset) b
       , b2     <- move (Lens lowerFloors) (Lens higherFloors) b1
                ++ move (Lens higherFloors) (Lens lowerFloors) b1
       , b3     <- updateCurrentFloor (floorUnion subset) b2
       ]

updateCurrentFloor :: (Floor -> Floor) -> Building -> [Building]
updateCurrentFloor f b =
  [ b' | let (fl',b') = b & currentFloor <%~ f, isValidFloor fl' ]

{-# INLINE move #-}
move ::
  ReifiedLens' Building [Floor] ->
  ReifiedLens' Building [Floor] ->
  Building ->
  [Building]
move (Lens back) (Lens front) b =
  [ b & back         %~ cons (b^.currentFloor)
      & currentFloor .~ x
      & front        .~ xs
  | x:xs <- [ b^.front ]
  ]

-- | Characterize a 4-floor building with up to 7 generator/chip pairs
mkRep :: Building -> Int
mkRep (Building _ x _ z) = foldl' aux (length x) (x ++ z)
  where
    aux acc fl = acc `shiftL` 14 .|. floorRep fl
