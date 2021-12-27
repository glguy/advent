{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

myInput :: Vector.Vector Bool
myInput = Vector.fromList (toBool <$> "01111001100111011")

toBool :: Char -> Bool
toBool x = x == '1'

fromBool :: Bool -> Char
fromBool x = if x then '1' else '0'

part1, part2 :: Int
part1 = 272
part2 = 35651584

expand :: Int -> Vector Bool -> Vector Bool
expand n seed
  | Vector.length seed >= n = Vector.take n seed
  | otherwise = expand n
              $ seed <> Vector.singleton False <>
                Vector.map not (Vector.reverse seed)

checksum v
  | odd n     = fromBool <$> Vector.toList v
  | otherwise = checksum
              $ Vector.generate (n`quot`2) $ \i ->
                   v Vector.! (2*i) == v Vector.! (2*i+1)
  where
    n = Vector.length v

main :: IO ()
main =
 do putStrLn (checksum (expand part1 myInput))
    putStrLn (checksum (expand part2 myInput))
