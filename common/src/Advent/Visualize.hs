{-|
Module      : Advent.Visualize
Description : Module for visualizing components of the solutions
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

Thin wrapper around the JuicyPixels library to make it easy to generate
small animations from solution files.

-}
module Advent.Visualize
  ( Image
  , PixelRGB8(..)

  , writePng
  , writeAnimation
  , generateImage

  , coordImage

  , colorWheel
  ) where

import Advent.Coord
import Codec.Picture
import Data.Word (Word8)

-- | Generate an image given coordinate bounds and a projection from coordinates to colors.
coordImage ::
  Pixel p =>
  (Coord, Coord) {- ^ inclusive coordinate range -} ->
  (Coord -> p)   {- ^ pixel coloring function -} ->
  Image p
coordImage (C loy lox, C hiy hix) f = generateImage toPixel width height
  where
    toPixel x y = f (C (loy+y) (lox+x))
    width       = hix - lox + 1
    height      = hiy - loy + 1

-- | Assign a gradient rainbow to the values of a 8-bit number.
colorWheel :: Word8 -> PixelRGB8
colorWheel i
  | i < 85    = PixelRGB8 (255 - i * 3) 0 (i * 3)
  | i < 170   = PixelRGB8 0 ((i-85) * 3) (255 - (i-85)*3)
  | otherwise = PixelRGB8 ((i-170) * 3) (255 - (i-170)*3) 0

-- | Save a looping animated GIF to disk given the animation frames and a delay.
writeAnimation ::
  FilePath          {- ^ output filename -} ->
  Int               {- ^ frame delay in centiseconds -} ->
  [Image PixelRGB8] {- ^ animation frames -} ->
  IO ()
writeAnimation path delay imgs =
  case writeGifAnimation path delay LoopingForever imgs of
    Left e -> fail e
    Right io -> io
