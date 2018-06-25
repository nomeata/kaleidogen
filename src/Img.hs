module Img where

import Data.Complex
import Data.Colour
import Data.Colour.CIE
import Data.Colour.SRGB
import Data.Colour.Names

import RNA

-- Function evaluator (not actually used at the moment)

type Img = Complex Double -> Colour Double

scale :: Img -> Img
scale i c = i (c / 0.8)

mix :: (Complex Double -> Bool) -> Img -> Img -> Img
mix p i1 i2 c = if p c then i1 c else i2 c

brightness :: Colour Double -> Double
brightness c = (luminance c) ^ 2

toImg :: RNA -> Img
toImg (Op0 o)          = op0Img o
toImg (Op1 o i1)       = op1Img o (toImg i1)
toImg (Op2 o i1 i2)    = op2Img o (toImg i1) (toImg i2)
toImg (Op3 o i1 i2 i3) = op3Img o (toImg i1) (toImg i2) (toImg i3)

op0Img :: Op0 -> Img
op0Img (Solid col) = f
  where f c | magnitude c > 1 = black
            | otherwise       = col
op0Img Gradient = f
  where f c | magnitude c > 1 = black
            | otherwise       = blend (1 - magnitude c) black white

op2Img Before fb bg = mix ((<0.8) . magnitude) (scale fb) bg
op2Img (Rays n) i1 i2 = mix (\c -> even (round (phase c / pi * fromIntegral n))) i1 i2
op2Img Checker i1 i2 = mix f i1 i2
  where f c = let c' = c * cis (pi/4) * 6
              in even (round (realPart c') + round (imagPart c'))

op1Img Inv i = \c -> i $
    let (m,p) = polar c in
    if m > 1 then c else mkPolar (1 - m) p
op1Img (Swirl x) i = \c -> i $
    let (m,p) = polar c in
    mkPolar m (p + (1-m) * x)

op3Img Blur i1 i2 i3 = \c ->
    blend (brightness (i1 c)) (i2 c) (i3 c)


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

