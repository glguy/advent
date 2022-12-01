{-# Language QuasiQuotes, BlockArguments, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/6>

Apply light on/off/toggle commands to a grid.

-}
module Main where

import Advent (format, countBy, stageTH)
import Advent.Coord ( Coord(..) )
import Control.Monad.ST ( ST, runST )
import Data.Array.ST
import Data.Foldable ( for_, traverse_ )

data C = Con | Coff | Ctoggle
data Command = Command !C Coord Coord

stageTH

-- | >>> :main
-- 377891
-- 14110788
main :: IO ()
main =
 do input <- [format|2015 6 ((turn |)@C %d,%d through %d,%d%n)*|]
    let cmds = [Command c (C y1 x1) (C y2 x2) | (c,x1,y1,x2,y2) <- input]
    print (runST (part1 cmds))
    print (runST (part2 cmds))

part1 :: [Command] -> ST s Int
part1 cmds =
 do a <- newBitGrid
    traverse_ (bitCommand a) cmds
    xs <- getElems a
    return $! countBy id xs

part2 :: [Command] -> ST s Int
part2 cmds =
 do a <- newIntGrid
    traverse_ (intCommand a) cmds
    xs <- getElems a
    return $! sum xs

bitCommand :: STUArray s Coord Bool -> Command -> ST s ()
bitCommand a (Command op x y) =
  for_ (range (x, y)) \p ->
    case op of
      Con     -> writeArray a p True
      Coff    -> writeArray a p False
      Ctoggle -> writeArray a p . not =<< readArray a p

intCommand :: STUArray s Coord Int -> Command -> ST s ()
intCommand a (Command op x y) =
  for_ (range (x, y)) \p ->
    writeArray a p . upd =<< readArray a p
  where
  upd = case op of
          Con     -> (+1)
          Coff    -> max 0 . subtract 1
          Ctoggle -> (+2)

newBitGrid :: ST s (STUArray s Coord Bool)
newBitGrid = newArray (C 0 0, C 999 999) False

newIntGrid :: ST s (STUArray s Coord Int)
newIntGrid = newArray (C 0 0, C 999 999) 0
