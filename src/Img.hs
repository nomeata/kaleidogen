{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{- # OPTIONS_GHC -ddump-simpl # -}
module Img (Img, toImg, img2Juicy)  where

import Data.Complex
import Data.Colour.SRGB

import Codec.Picture

import RNA

-- Functional evaluator

#ifdef __GHCJS__
import qualified Data.Fixed

fmod :: Double -> Double -> Double
fmod = Data.Fixed.mod'
#else
-- more efficient than Data.Fixed.fmod (I believe)
foreign import ccall unsafe "math.h fmod" c_fmod :: Double -> Double -> Double

fmod :: Double -> Double -> Double
x `fmod` y =
  let z = c_fmod x y in
  if z < 0 then z + y else z
#endif

data C = C {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

type Img = Complex Double -> Maybe C
type Pattern = Complex Double -> C

toImg :: RNA -> Img
toImg r pos =
    if magnitude pos > 1.0
    then Nothing
    else Just (i pos)
  where
    i = go r

tween :: Double -> C -> C -> C
tween x (C a1 b1 c1) (C a2 b2 c2) = C
  ((1 - x) * a1 + x * a2)
  ((1 - x) * b1 + x * b2)
  ((1 - x) * c1 + x * c2)

go :: RNA -> Pattern
go _ (!_pos) | id False = undefined

go (Solid color) _pos = C r g b
  where
    RGB r g b = color

go Blend{} _pos = error "blend"

go (Checker x r1 r2) pos = do
    let !(tmpx :+ tmpy) = realToFrac x * ((1.0 :+ 0.0) + pos)
    if abs ((tmpx + 1) `fmod` 2 - 1) + abs (tmpy `fmod` 2 - 1) < 1
      then i2 pos
      else i1 pos
  where
    i1 = go r1
    i2 = go r2

go (Rotate x r1) pos = do
    let p' = phase pos + x
    let pos' = mkPolar (magnitude pos) p'
    i1 pos'
  where
    i1 = go r1

go (Invert r1) pos = do
    let mag = magnitude pos
    let mag' = 1 - mag
    let pos' = mkPolar mag' (phase pos)
    i1 pos'
  where
    i1 = go r1

go (Swirl x r1) pos = do
    let mag = magnitude pos
    let phase' = phase pos + (1.0 - mag) * x
    let pos' = mkPolar mag phase'
    i1 pos'
  where
    i1 = go r1

go (Dilated r r1) pos = do
    let p = phase pos
    let mag = magnitude pos
    let phase' = p + 1/fromIntegral r * sin (p * fromIntegral r)
    let pos' = mkPolar mag phase'
    i1 pos'
  where
    i1 = go r1


go (Rays r r1 r2) pos =
    if (phase pos / pi * fromIntegral r + 0.5) `fmod` 2 < 1
    then i2 pos
    else i1 pos
  where
    i1 = go r1
    i2 = go r2

go (Gradient r1 r2) pos =
    tween (magnitude pos) (i1 pos) (i2 pos)
  where
    i1 = go r1
    i2 = go r2

go (Ontop x r1 r2) pos = do
    let pos1 = realToFrac (1/x) * pos
    if magnitude pos < x
    then i1 pos1 else i2 pos
  where
    i1 = go r1
    i2 = go r2

img2Juicy :: Int -> Img -> Image PixelRGBA8
img2Juicy dim i = generateImage colorAt w h
  where
    w = dim
    h = dim
    colorAt :: Int -> Int -> PixelRGBA8
    colorAt x y = do
      let x' = 2 * fromIntegral x / fromIntegral w - 1
      let y' = 2 * fromIntegral y / fromIntegral h - 1
      case i (y' :+ x') of
        Nothing -> PixelRGBA8 0 0 0 0
        Just (C r g b) -> PixelRGBA8 (clamp r) (clamp g) (clamp b) 255

    clamp :: Double -> Pixel8
    clamp = fromIntegral . max (0::Int) . min 255 . round . (* 256)
