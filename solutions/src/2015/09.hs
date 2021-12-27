module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ( Map )
import Data.List

data Edge = Edge String String    deriving (Eq, Ord, Show, Read)

edge :: String -> String -> Edge
edge x y
  | x < y     = Edge x y
  | otherwise = Edge y x

edgeParts :: Edge -> [String]
edgeParts (Edge x y) = [x,y]

main :: IO ()
main =
  do input <- loadInput "input9.txt"
     let places = uniques (concatMap edgeParts (Map.keys input))
         costs  = tripLength input <$> permutations places
     print (minimum costs)
     print (maximum costs)

loadInput :: FilePath -> IO (Map Edge Int)
loadInput filename = Map.fromList . map parse . lines <$> readFile filename

parse :: String -> (Edge, Int)
parse ln =
  case words ln of
    [x,"to",y,"=",z] -> (edge x y,read z)
    _                -> error ("Bad line: " ++ ln)

tripLength :: Map Edge Int -> [String] -> Int
tripLength m xs = sum (zipWith edgeLength xs (tail xs))
  where
    edgeLength x y = m Map.! edge x y

uniques :: Ord a => [a] -> [a]
uniques = Set.toList . Set.fromList
