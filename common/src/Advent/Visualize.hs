{-|
Module      : Advent.Visualize
Description : Module for visualizing components of the solutions
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

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

coordImage :: Pixel p => (Coord, Coord) -> (Coord -> p) -> Image p
coordImage (C loy lox, C hiy hix) f = generateImage toPixel width height
  where
    toPixel x y = f (C (loy+y) (lox+x))
    width       = hix - lox + 1
    height      = hiy - loy + 1

colorWheel :: Word8 -> PixelRGB8
colorWheel i
  | i < 85    = PixelRGB8 (255 - i * 3) 0 (i * 3)
  | i < 170   = PixelRGB8 0 ((i-85) * 3) (255 - (i-85)*3)
  | otherwise = PixelRGB8 ((i-170) * 3) (255 - (i-170)*3) 0

writeAnimation :: FilePath -> Int -> [Image PixelRGB8] -> IO ()
writeAnimation path delay imgs =
  case writeGifAnimation path delay LoopingForever imgs of
    Left e -> fail e
    Right io -> io
