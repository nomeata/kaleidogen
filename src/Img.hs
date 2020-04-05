{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{- # OPTIONS_GHC -ddump-simpl # -}
module Img where

import Data.Complex
import Data.Colour
import Data.Colour.CIE
import Data.Colour.SRGB
import Data.Colour.Names
import qualified Data.ByteString.Lazy as BS
import GHC.Exts

import Codec.Picture

import RNA

-- Functional evaluator

{-
div' :: Double -> Double -> Int
div' n d = floor (toRational n / toRational d)
{-# INLINE div' #-}

mod' :: Double -> Double -> Double
mod' n d = n - fromIntegral f * d where
    f = div' n d
{-# INLINE mod' #-}
-}

foreign import ccall unsafe "math.h fmod" fmod :: Double -> Double -> Double

x `mod'` y = let z = fmod x y in
  if z < 0 then z + y else z

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
go _ (!pos) | False = undefined

go (Solid color) _pos = C r g b
  where
    RGB r g b = color

go (Blend x r1 r2) _pos = error "blend"

go (Checker x r1 r2) pos = do
    let !(tmpx :+ tmpy) = realToFrac x * ((1.0 :+ 0.0) + pos)
    if abs ((tmpx + 1) `mod'` 2 - 1) + abs (tmpy `mod'` 2 - 1) < 1
      then i1 pos
      else i2 pos
  where
    i1 = go r1
    i2 = go r2

go (Rotate x r1) pos = do
    let p' = phase pos
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
    if (phase pos / pi * fromIntegral r + 0.5) `mod'` 2 < 1
    then i1 pos
    else i2 pos
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



img2Png :: Img -> BS.ByteString
img2Png i = encodePng $ generateImage go w h
  where
    w = 1000
    h = 1000
    go :: Int -> Int -> PixelRGBA8
    go x y = do
      let x' = 2 * fromIntegral x / fromIntegral w - 1
      let y' = 2 * fromIntegral y / fromIntegral h - 1
      case i (x' :+ y') of
        Nothing -> PixelRGBA8 0 0 0 0
        Just (C x y z) -> PixelRGBA8 (clamp x) (clamp y) (clamp z) 255

    clamp :: Double -> Pixel8
    clamp = fromIntegral . max (0::Int) . min 255 . round . (* 256)

-- For reference: Parts of the rendering code

{-
render :: Int -> Int -> Img -> BS.ByteString
render w h f = BS.pack $ concat
    [ [fromIntegral r,fromIntegral g,fromIntegral b,0xff]
    | y <- [0..h-1]
    , x <- [0..w-1]
    , let x' = s * fromIntegral (x - (w`div`2))
    , let y' = s * fromIntegral (y - (h`div`2))
    , let RGB r g b = toSRGB24 $ f (x' :+ y')
    ]
  where s = 2 / min (fromIntegral w) (fromIntegral h)

paint :: JSVal -> Img -> JSM ()
paint canvas f = do
    Just w <- canvas ^. js "width" >>= fromJSVal
    Just h <- canvas ^. js "height" >>= fromJSVal
    --jsg "console" ^. js1 "log" (w,h)
    ctx <- canvas ^. js1 "getContext" "2d"
    (buf, _, _) <- ghcjsPure $ B.fromByteString $ render w h f
    array <- ghcjsPure (B.getUint8Array buf)
    array <- ghcjsPure (jsval array)
    array <- new (jsg "Uint8ClampedArray") array
    imgData <- new (jsg "ImageData") (array,w,h)
    ctx ^. js3 "putImageData" imgData (0::Int) (0::Int)
    return ()
-}

