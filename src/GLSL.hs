{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module GLSL where

import Prelude hiding (unlines)
import Data.Monoid
import Formatting.Buildable
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder
import Data.Colour.SRGB
import RNA

toFragmentShader :: RNA -> T.Text
toFragmentShader rna = TL.toStrict $ toLazyText $ conclude $ go rna 0

unlines :: [Builder] -> Builder
unlines = foldMap (<> "\n")

conclude :: (Builder, Integer) -> Builder
conclude (pgm, r) = unlines
  [ "precision mediump float;"
  , "uniform vec4 u_extraData;"
  , "varying vec2 vDrawCoord;"
  , "void main() {"
  , "  float extraData = u_extraData.x;"
  , "  vec2 pos0 = vDrawCoord;"
  , "  if (length(pos0) > 1.0) { gl_FragColor = vec4(0,0,0,0.0); return; }"
  , "  if (extraData > 0.5) {" -- need a hightlighting border
  , "    if (length(pos0) > 0.9) {"
  , "      if (extraData > 1.5) {"
  , "        gl_FragColor = vec4(0,0,1.0,1.0);"
  , "      } else {"
  , "        gl_FragColor = vec4(0,0,0,0.0);"
  , "      };"
  , "      return;"
  , "    }"
  , "    pos0 = pos0 / 0.9;"
  , "  }"
  , pgm
  , "  gl_FragColor = vec4(" <> col r <> ", 1.0);"
  , "}"
  ]

blankShader :: T.Text
blankShader = TL.toStrict $ toLazyText $ conclude $ go (Solid (RGB 1 1 1)) 0

-- GLSL evaluator

-- | A value of type GLSLGen is a function that take a integer $n$
-- and produces
--
--  * a number $m$, $m ≥ n$.
--  * lines of a GLSL progam that reads a coordinate in variable posn,
--    and writes a color to the variable posm
--
-- This could all be much nicer, and heavy refactoring is welcome.
type GLSLGen = Integer -> (Builder, Integer)

pos :: Integer -> Builder
pos n = "pos" <> build n
col :: Integer -> Builder
col n = "col" <> build n
tmp :: Integer -> Builder
tmp n = "tmp" <> build n
tmp2 :: Integer -> Builder
tmp2 n = "tmp" <> build n <> "_2"
tmp3 :: Integer -> Builder
tmp3 n = "tmp" <> build n <> "_3"

floatEq :: Builder -> Builder -> Builder
x `floatEq` y = "float " <> x <> " = " <> y <> ";"
vec2Eq :: Builder -> Builder -> Builder
x `vec2Eq` y = "vec2 " <> x <> " = " <> y <> ";"
vec3Eq :: Builder -> Builder -> Builder
x `vec3Eq` y = "vec3 " <> x <> " = " <> y <> ";"

phase, len :: Builder -> Builder
phase x = "atan(" <> x <> ".x, " <> x <> ".y)"
len x = "length(" <> x <> ")"
mkPolar :: Builder -> Builder -> Builder
mkPolar l x = "(" <> l <> " * vec2(cos(" <> x <> "),sin(" <> x <> ")))"
float :: Int -> Builder
float x = build (fromIntegral x :: Double)

go :: RNA -> GLSLGen

go (Solid color) n = (,n) $ unlines
    [ col n `vec3Eq` ("vec3(" <> build r <> "," <> build g <> "," <> build b <> ")")
    ]
  where RGB r g b = color

go (Blend x r1 r2) n = (,n2+1) $ unlines
    [ pos (n+1) `vec2Eq` pos n
    , src1
    , pos (n1+1) `vec2Eq` pos n
    , src2
    , col (n2+1) `vec3Eq` (build x <> " * " <> col n1 <> " + (1.0-" <> build x <> ") * " <> col n2)
    ]
  where
    (src1, n1) = go r1 (n+1)
    (src2, n2) = go r2 (n1+1)

go (Checker x r1 r2) n = (,n2+1) $ unlines
    [ pos (n+1) `vec2Eq` pos n
    , src1
    , pos (n1+1) `vec2Eq` pos n
    , src2
    , tmp n `vec2Eq` (build x <> "*(1.0/sqrt(2.0)) * mat2(1.0,1.0,-1.0,1.0) * " <> pos n)
    , "vec3 " <> col (n2+1) <> ";"
    , "if (mod(" <> tmp n <> ".x, 2.0) < 1.0 != mod(" <> tmp n <> ".y, 2.0) < 1.0) {"
    , "   " <> col (n2+1) <> " = " <> col n1 <> ";"
    , "} else {"
    , "   " <> col (n2+1) <> " = " <> col n2 <> ";"
    , "};"
    ]
  where
    (src1, n1) = go r1 (n+1)
    (src2, n2) = go r2 (n1+1)

go (Rotate x r1) n = (,n1) $ unlines
    [ tmp n `floatEq` phase (pos n)
    , tmp2 n `floatEq` len (pos n)
    , tmp3 n `floatEq` (tmp n <> " + " <> build x)
    , pos (n+1) `vec2Eq` mkPolar (tmp2 n) (tmp3 n)
    , src1
    ]
  where
    (src1, n1) = go r1 (n+1)

go (Invert r1) n = (,n1) $ unlines
    [ pos (n+1) `vec2Eq` ("((1.0-length(" <> pos n <> "))/length(" <> pos n <> ")) * " <> pos n)
    , src1
    ]
  where
    (src1, n1) = go r1 (n+1)

go (Swirl x r1) n = (,n1) $ unlines
    [ tmp n `floatEq` phase (pos n)
    , tmp2 n `floatEq` len (pos n)
    , tmp3 n `floatEq` (tmp n <> " + (1.0 - " <> tmp2 n <> ") * " <> build x)
    , pos (n+1) `vec2Eq` mkPolar (tmp2 n) (tmp3 n)
    , src1
    ]
  where
    (src1, n1) = go r1 (n+1)

go (Dilated r r1) n = (,n1) $ unlines
    [ tmp n `floatEq` phase (pos n)
    , tmp2 n `floatEq` len (pos n)
    , tmp3 n `floatEq` (tmp n <> " + 1.0/" <> float r <> " * sin(" <> tmp n <> " * " <> float r <> ")")
    , pos (n+1) `vec2Eq` mkPolar (tmp2 n) (tmp3 n)
    , src1
    ]
  where
    (src1, n1) = go r1 (n+1)

go (Rays r r1 r2) n = (,n2+1) $ unlines
    [ src1
    , pos (n1+1) `vec2Eq` pos n
    , src2
    , "vec3 " <> col (n2+1) <> ";"
    , "if (mod( " <> phase (pos n) <> "/" <> build (pi :: Double) <> " * " <> float r <> ", 2.0) < 1.0) {"
    , "   " <> col (n2+1) <> " = " <> col n1 <> ";"
    , "} else {"
    , "   " <> col (n2+1) <> " = " <> col n2 <> ";"
    , "};"
    ]
  where
    (src1, n1) = go r1 n
    (src2, n2) = go r2 (n1+1)

go (Gradient r1 r2) n = (,n2+1) $ unlines
    [ pos (n+1) `vec2Eq` pos n
    , src1
    , pos (n1+1) `vec2Eq` pos n
    , src2
    , tmp2 n `floatEq` len (pos n)
    , col (n2+1) `vec3Eq` (tmp2 n <> " * " <> col n1 <> " + (1.0 - " <> tmp2 n <> ") * " <> col n2)
    ]
  where
    (src1, n1) = go r1 (n+1)
    (src2, n2) = go r2 (n1+1)

go (Ontop x r1 r2) n = (,n2+1) $ unlines
    [ pos (n+1) `vec2Eq` (pos n <> "/" <> build x)
    , src1
    , pos (n1+1) `vec2Eq` pos n
    , src2
    , "vec3 " <> col (n2+1) <> ";"
    , "if (length(" <> pos n <> ") < " <> build x <> ") {"
    , "   " <> col (n2+1) <> " = " <> col n1 <> ";"
    , "} else {"
    , "   " <> col (n2+1) <> " = " <> col n2 <> ";"
    , "};"
    ]
  where
    (src1, n1) = go r1 (n+1)
    (src2, n2) = go r2 (n1+1)
