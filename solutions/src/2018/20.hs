{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/20>

This solution relies on the generated maze starting from the origin.
Each door will be located at coordinates where one component of the
coordinate is odd and the other is even. Rooms will be located at
coordinates where both components are even. Coordinates where both
components are odd will always be walls.

@
    -  <->  +
   54321012345
  5###########
- 4#.|.#.|.#.#
  3#-###-#-#-#
  2#.|.|.#.#.#
^ 1#-#####-#-#
| 0#.#.#X|.#.#
v 1#-#-#####-#
  2#.#.|.|.|.#
  3#-###-###-#
+ 4#.|.|.#.|.#
  5###########
@

-}
module Main (main) where

import Advent (countBy, getInputLines)
import Advent.Coord (Coord(C), above, below, left, right, origin)
import Advent.Search (bfsOn)
import Control.Applicative (Alternative((<|>), many))
import Control.Monad (foldM)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.ParserCombinators.ReadP (ReadP, char, between, readP_to_S, sepBy1)

-- | Cardinal directions: north south east west
data Dir = N | S | E | W
  deriving (Eq, Ord, Show)

-- | Regular expressions parameterized by the underlying elements
newtype Regexp a = RE [[Either a (Regexp a)]]
  deriving (Eq, Ord, Show)

-- | Print the answers to day 20
--
-- >>> :main
-- 4121
-- 8636
main :: IO ()
main =
 do [input] <- getInputLines 2018 20
    let [(re,_)] = readP_to_S parseRe0 input 
    let (doors, _) = route (Set.singleton origin) re
    let ds = distances (neighbor doors) (C 0 0)

    print (maximum ds)
    print (countBy (>= 1000) ds)

-- Regular expression parsing for each level of precedence
parseRe0 :: ReadP (Regexp Dir)
parseRe0 = between (char '^') (char '$') parseRe1

parseRe1 :: ReadP (Regexp Dir)
parseRe1 = RE <$> many parseRe2 `sepBy1` char '|'

parseRe2 :: ReadP (Either Dir (Regexp Dir))
parseRe2 = Right <$> between (char '(') (char ')') parseRe1 <|> Left <$> parseDir

-- | Parse a cardinal direction
parseDir :: ReadP Dir
parseDir = N <$ char 'N' <|> S <$ char 'S' <|> E <$ char 'E' <|> W <$ char 'W'

-- | Move one space giving a direction and a starting coordinate
move :: Dir -> Coord -> Coord
move N = above
move S = below
move W = left
move E = right

-- | Given the set of doors, generate a list of rooms reachable from a
-- given room
neighbor :: Set Coord -> Coord -> [Coord]
neighbor doors here =
  [ move dir (move dir here)
    | dir <- [N,E,S,W]
    , Set.member (move dir here) doors ]

data WithLen a = WithLen { dist :: !Int, loc :: a }

-- | Given a neighbors generating function compute the minimum distances
-- to all reachable locations.
distances :: Ord a => (a -> [a]) -> a -> [Int]
distances next start = dist <$> bfsOn loc (\(WithLen x y) -> WithLen (x+1) <$> next y) (WithLen 0 start)

-- | Given a regular expression, compute a set of generated doors and end points
-- generated from the regular expression when starting at the origin.
route :: Set Coord -> Regexp Dir -> (Set Coord, Set Coord)
route starts (RE alts) = foldMap (foldM routeFrom starts) alts

-- Given a set of starting points and a new direction or sub-expression
-- compute the reachable doors and the ending coordinates
routeFrom :: Set Coord -> Either Dir (Regexp Dir) -> (Set Coord, Set Coord)
routeFrom starts = either (dirStep starts) (route starts)

-- | Generate the door passed thorugh and the end point when taking a step from the origin
-- in the given direction.
dirStep :: Set Coord -> Dir -> (Set Coord, Set Coord)
dirStep starts d = ( Set.mapMonotonic (move d) starts -- doors
                   , Set.mapMonotonic (move d . move d) starts) -- endpoint
