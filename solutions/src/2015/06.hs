module Main where

import Data.Array.ST
import Data.Foldable
import Control.Monad.ST

data Op = On | Off | Toggle
type Point = (Int,Int)
data Command = Command !Op Point Point

main :: IO ()
main =
  do cmds <- loadInput
     print (runST (part1 cmds))
     print (runST (part2 cmds))

part1 :: [Command] -> ST s Int
part1 cmds =
  do a <- newBitGrid
     traverse_ (bitCommand a) cmds
     xs <- getElems a
     return $! length (filter id xs)

part2 :: [Command] -> ST s Int
part2 cmds =
  do a <- newIntGrid
     traverse_ (intCommand a) cmds
     xs <- getElems a
     return $! sum xs

bitCommand :: STUArray s Point Bool -> Command -> ST s ()
bitCommand a (Command op x y) =
  forRange x y $ \p ->
    case op of
      On     -> writeArray a p True
      Off    -> writeArray a p False
      Toggle -> writeArray a p . not =<< readArray a p

intCommand :: STUArray s Point Int -> Command -> ST s ()
intCommand a (Command op x y) =
  forRange x y $ \p ->
    writeArray a p . upd =<< readArray a p
  where
  upd = case op of
          On     -> (+1)
          Off    -> max 0 . subtract 1
          Toggle -> (+2)

forRange :: Applicative m => Point -> Point -> (Point -> m a) -> m ()
forRange (xlo,ylo) (xhi,yhi) k =
  for_ [xlo..xhi] $ \x ->
  for_ [ylo..yhi] $ \y ->
  k (x,y)
{-# INLINE forRange #-}

loadInput :: IO [Command]
loadInput = map parseLine . lines <$> readFile "input6.txt"

newBitGrid :: ST s (STUArray s Point Bool)
newBitGrid = newArray ((0,0),(999,999)) False

newIntGrid :: ST s (STUArray s Point Int)
newIntGrid = newArray ((0,0),(999,999)) 0

parseLine :: String -> Command
parseLine str = Command op (parsePoint p1) (parsePoint p2)
  where
  (op,p1,p2) =
    case words str of
      ["turn","on", x,"through",y] -> (On,     x, y)
      ["turn","off",x,"through",y] -> (Off,    x, y)
      ["toggle",    x,"through",y] -> (Toggle, x, y)
      _ -> error ("Bad line: " ++ str)

parsePoint :: String -> Point
parsePoint str =
  foldr const (error ("Bad point: " ++ str)) $
  do (x,',':ystr) <- reads str
     (y,[]      ) <- reads ystr
     return (x,y)
