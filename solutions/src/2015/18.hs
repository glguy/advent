module Main where

import Data.Array.Unboxed

type Lights = UArray (Int,Int) Bool

main :: IO ()
main =
  do input <- loadInput
     let steps = 100
     print $ countLights $ iterate (applyRule life) input !! steps
     print $ countLights $ iterate (applyRule (addCorners life)) input !! steps

countLights :: Lights -> Int
countLights = length . filter id . elems

loadInput :: IO Lights
loadInput =
  do str <- readFile "input18.txt"
     let lights = map (map (=='#')) (lines str)
     return $! case lights of
       []  -> array ((1,1),(0,0)) []
       x:_ -> array ((1,1),(length lights, length x))
                [ ((r,c), col)
                  | (r,row) <- zip [1..] lights
                  , (c,col) <- zip [1..] row ]

type Rule = Lights -> (Int,Int) -> Bool

applyRule :: Rule -> Lights -> Lights
applyRule f a = array (bounds a) [ (i, f a i) | i <- range (bounds a) ]

life :: Rule
life a i@(x,y) = neighbors == 3 ||
                 neighbors == 2 && a!i
  where
  neighbors = length [ () | x' <- [x-1..x+1], y' <- [y-1..y+1]
                          , let i' = (x',y')
                          , i /= i'
                          , inRange (bounds a) i'
                          , a ! i'
                          ]

addCorners :: Rule -> Rule
addCorners f a i@(x,y)
  | x == xlo || x == xhi
  , y == ylo || y == yhi = True
  | otherwise            = f a i
  where
  ((xlo,ylo),(xhi,yhi)) = bounds a
