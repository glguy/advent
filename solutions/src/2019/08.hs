{-# Language QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/8>

-}
module Main (main) where

import Advent              (count, chunks, format)
import Control.Applicative ((<|>),many)
import Data.List           (minimumBy)
import Data.Ord            (comparing)

type Layer = [[P]]

data P = P0 | P1 | P2
  deriving (Eq, Ord, Show)

mempty

-- | >>> :main
-- 2080
-- ░██░░█░░█░███░░░██░░█░░░█
-- █░░█░█░░█░█░░█░█░░█░█░░░█
-- █░░█░█░░█░█░░█░█░░░░░█░█░
-- ████░█░░█░███░░█░░░░░░█░░
-- █░░█░█░░█░█░█░░█░░█░░░█░░
-- █░░█░░██░░█░░█░░██░░░░█░░
main :: IO ()
main =
  do inp <- [format|8 @P*%n|]
     let layers = chunks 6 (chunks 25 inp)
     print (part1 layers)
     mapM_ (putStrLn . map render) (overlayLayers layers)

render :: P -> Char
render P0 = '\x2591'
render P1 = '\x2588'
render P2 = '\x2592'

overlayLayers :: [Layer] -> Layer
overlayLayers = foldr1 (zipWith (zipWith overlay))

overlay :: P -> P -> P
overlay P2 x = x
overlay x  _ = x

part1 :: [Layer] -> Int
part1 layers = count P1 layer * count P2 layer
  where
    layer = minimumBy (comparing (count P0))
          $ map concat layers
