{-# LANGUAGE BlockArguments #-}
module Main where

import Advent ( getInputLines, countBy )
import Advent.Coord ( Coord(..) )
import Control.Monad.ST ( ST, runST )
import Data.Array.ST
import Data.Foldable ( for_, traverse_ )

data Op = On | Off | Toggle
data Command = Command !Op Coord Coord

main :: IO ()
main =
 do cmds <- map parseLine <$> getInputLines 6
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
      On     -> writeArray a p True
      Off    -> writeArray a p False
      Toggle -> writeArray a p . not =<< readArray a p

intCommand :: STUArray s Coord Int -> Command -> ST s ()
intCommand a (Command op x y) =
  for_ (range (x, y)) \p ->
    writeArray a p . upd =<< readArray a p
  where
  upd = case op of
          On     -> (+1)
          Off    -> max 0 . subtract 1
          Toggle -> (+2)

newBitGrid :: ST s (STUArray s Coord Bool)
newBitGrid = newArray (C 0 0, C 999 999) False

newIntGrid :: ST s (STUArray s Coord Int)
newIntGrid = newArray (C 0 0, C 999 999) 0

parseLine :: String -> Command
parseLine str = Command op (parsePoint p1) (parsePoint p2)
  where
  (op,p1,p2) =
    case words str of
      ["turn","on", x,"through",y] -> (On,     x, y)
      ["turn","off",x,"through",y] -> (Off,    x, y)
      ["toggle",    x,"through",y] -> (Toggle, x, y)
      _ -> error ("Bad line: " ++ str)

parsePoint :: String -> Coord
parsePoint str =
  foldr const (error ("Bad point: " ++ str)) $
  do (x,',':ystr) <- reads str
     (y,[]      ) <- reads ystr
     return (C y x)
