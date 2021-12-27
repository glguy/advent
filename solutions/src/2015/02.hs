module Main where

import Data.List
import Data.List.Split
import Text.Read

data Package = Package Int Int Int
data Face = Face Int Int

main :: IO ()
main =
  do packages <- loadInput
     print (sum (part1 <$> packages))
     print (sum (part2 <$> packages))

loadInput :: IO [Package]
loadInput = map parseLine . lines <$> readFile "input2.txt"

parseLine :: String -> Package
parseLine str =
  case traverse readMaybe (splitOn "x" str) of
    Just [x,y,z] -> Package x y z
    _            -> error ("bad line: " ++ str)

part1 :: Package -> Int
part1 p = surfaceArea p + area (smallestFace p)

part2 :: Package -> Int
part2 p = volume p + perimeter (smallestFace p)

volume :: Package -> Int
volume (Package x y z) = x*y*z

surfaceArea :: Package -> Int
surfaceArea (Package x y z) = 2 * (x*y + x*z + y*z)

smallestFace :: Package -> Face
smallestFace (Package x y z) = let a:b:_ = sort [x,y,z] in Face a b

area :: Face -> Int
area (Face x y) = x*y

perimeter :: Face -> Int
perimeter (Face x y) = 2*(x+y)
