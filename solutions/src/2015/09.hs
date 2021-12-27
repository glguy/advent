{-# Language QuasiQuotes, ImportQualifiedPost #-}
module Main where

import Advent.Format (format)
import Data.List (permutations)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

data Edge = Edge String String    deriving (Eq, Ord, Show, Read)

edge :: String -> String -> Edge
edge x y
  | x < y     = Edge x y
  | otherwise = Edge y x

main :: IO ()
main =
 do input <- [format|9 (%s to %s = %u%n)*|]
    let graph = Map.fromList [(edge x y, d) | (x,y,d) <- input]    
        places = uniques [z | (x,y,_) <- input, z <- [x,y]]
        costs  = tripLength graph <$> permutations places
    print (minimum costs)
    print (maximum costs)

tripLength :: Map Edge Int -> [String] -> Int
tripLength m xs = sum (zipWith edgeLength xs (tail xs))
  where
    edgeLength x y = m Map.! edge x y

uniques :: Ord a => [a] -> [a]
uniques = Set.toList . Set.fromList
