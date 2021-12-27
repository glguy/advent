{-# Language TemplateHaskell #-}
module Main (main) where

import Language.Haskell.TH
import Day23TH

program :: Int -> Int -> Int
program = $(compile =<< runIO loadInput)

main :: IO ()
main =
  do print (program 0 0)
     print (program 1 0)
