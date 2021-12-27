module Main where

import Control.Monad.Loop
import Control.Monad.Trans.Class
import Data.Array.ST
import Data.Array.Unboxed
import Data.List

main :: IO ()
main =
  do print (findHouse solve1)
     print (findHouse solve2)

findHouse :: UArray Int Int -> Maybe Int
findHouse = fmap fst . find ( (>= target) . snd ) . assocs

target :: Int
target = 36000000

solve1 :: UArray Int Int
solve1 = runSTUArray $
  do let top = target `quot` 10
     a <- newArray (1,top) 0
     exec_ $
       do elf   <- for 1   (<= top) succ
          house <- for elf (<= top) (+elf)
          lift $ do old <- readArray a house
                    writeArray a house (old + elf*10)
     return a

solve2 :: UArray Int Int
solve2 = runSTUArray $
  do let top = target `quot` 11
     a <- newArray (1,top) 0
     exec_ $
       do elf   <- for 1   (<=top) succ
          house <- for elf (<= min top (elf*50)) (+elf)
          lift $ do old <- readArray a house
                    writeArray a house (old + elf*11)
     return a
