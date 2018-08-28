{-# LANGUAGE TupleSections #-}
module GLSL where

import Text.Printf
import Data.Colour.SRGB
import RNA

indent :: String -> String
indent = unlines . map ("  " ++) . lines

toFragmentShader :: RNA -> String
toFragmentShader rna = conclude $ go rna 0

conclude :: (String, Integer) -> String
conclude (pgm, r) = unlines
  [ "precision mediump float;"
  , "uniform vec2 u_windowSize;"
  , "void main() {"
  , "  float s = 2.0 / min(u_windowSize.x, u_windowSize.y);"
  , "  vec2 pos0 = s * (gl_FragCoord.xy - 0.5 * u_windowSize);"
  , "  if (length(pos0) > 1.0) { gl_FragColor = vec4(0,0,0,0); return; }"
  , indent pgm
  , "  gl_FragColor = vec4(col" ++ show r ++ ", 1.0);"
  , "}"
  ]


-- GLSL evaluator

-- | A value of type GLSLGen is a function that take a integer $n$
-- and produces
--
--  * a number $m$, $m ≥ n$.
--  * lines of a GLSL progam that reads a coordinate in variable posn,
--    and writes a color to the variable posm
--
-- This could all be much nicer, and heavy refactoring is welcome.
type GLSLGen = Integer -> (String, Integer)

go :: RNA -> GLSLGen

go (Solid col) n = (,n) $
    printf "vec3 col%d = vec3(%f,%f,%f);\n" n r g b
  where RGB r g b = col

go (Blend x r1 r2) n = (,n2+1) $ unlines
    [ printf "vec2 pos%d = pos%d;" (n+1) n
    , src1
    , printf "vec2 pos%d = pos%d;" (n1+1) n
    , src2
    , printf "vec3 col%d = %f * col%d + (1.0-%f) * col%d;" (n2+1) x n1 x n2
    ]
  where
    (src1, n1) = go r1 (n+1)
    (src2, n2) = go r2 (n1+1)

go (Checker x r1 r2) n = (,n2+1) $ unlines
    [ printf "vec2 pos%d = pos%d;" (n+1) n
    , src1
    , printf "vec2 pos%d = pos%d;" (n1+1) n
    , src2
    , printf "vec2 tmp%d = %f*(1.0/sqrt(2.0)) * mat2(1.0,1.0,-1.0,1.0) * pos%d;" n x n
    , printf "vec3 col%d;" (n2+1)
    , printf "if (mod(tmp%d.x, 2.0) < 1.0 != mod(tmp%d.y, 2.0) < 1.0) {" n n
    , printf "   col%d = col%d;" (n2+1) n1
    , printf "} else {"
    , printf "   col%d = col%d;" (n2+1) n2
    , printf "};"
    ]
  where
    (src1, n1) = go r1 (n+1)
    (src2, n2) = go r2 (n1+1)

go (Rotate x r1) n = (,n1) $ unlines
    [ printf "vec2 pos%d = length(pos%d) * vec2(cos(atan(pos%d.x, pos%d.y) + %f),sin(atan(pos%d.x, pos%d.y) + %f));" (n+1) n n n x n n x
    , src1
    ]
  where
    (src1, n1) = go r1 (n+1)

go (Invert r1) n = (,n1) $ unlines
    [ printf "vec2 pos%d = ((1.0-length(pos%d))/length(pos%d)) * pos%d;" (n+1) n n n
    , src1
    ]
  where
    (src1, n1) = go r1 (n+1)

go (Swirl x r1) n = (,n1) $ unlines
    [ printf "vec2 pos%d = length(pos%d) * vec2(cos(atan(pos%d.x, pos%d.y) + (1.0-length(pos%d))*%f),sin(atan(pos%d.x, pos%d.y) + (1.0-length(pos%d))*%f));" (n+1) n n n n x n n n x
    , src1
    ]
  where
    (src1, n1) = go r1 (n+1)

go (Rays r r1 r2) n = (,n2+1) $ unlines
    [ src1
    , printf "vec2 pos%d = pos%d;" (n1+1) n
    , src2
    , printf "vec3 col%d;" (n2+1)
    , printf "if (mod(atan(pos%d.x, pos%d.y)/%f * %f, 2.0) < 1.0) {" n n (pi::Double) (fromIntegral r :: Double)
    , printf "   col%d = col%d;" (n2+1) n1
    , printf "} else {"
    , printf "   col%d = col%d;" (n2+1) n2
    , printf "};"
    ]
  where
    (src1, n1) = go r1 n
    (src2, n2) = go r2 (n1+1)

go (Gradient r1 r2) n = (,n2+1) $ unlines
    [ printf "vec2 pos%d = pos%d;" (n+1) n
    , src1
    , printf "vec2 pos%d = pos%d;" (n1+1) n
    , src2
    , printf "vec3 col%d = length(pos%d) * col%d + (1.0-length(pos%d)) * col%d;" (n2+1) n n1 n n2
    ]
  where
    (src1, n1) = go r1 (n+1)
    (src2, n2) = go r2 (n1+1)

go (Ontop x r1 r2) n = (,n2+1) $ unlines
    [ printf "vec2 pos%d = pos%d/0.8;" (n+1) n
    , src1
    , printf "vec2 pos%d = pos%d;" (n1+1) n
    , src2
    , printf "vec3 col%d;" (n2+1)
    , printf "if (length(pos%d) < %f) {" n x
    , printf "   col%d = col%d;" (n2+1) n1
    , printf "} else {"
    , printf "   col%d = col%d;" (n2+1) n2
    , printf "};"
    ]
  where
    (src1, n1) = go r1 (n+1)
    (src2, n2) = go r2 (n1+1)
