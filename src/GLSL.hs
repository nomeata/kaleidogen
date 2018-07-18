{-# LANGUAGE TupleSections #-}
module GLSL where

import Text.Printf
import Data.Colour.SRGB
import RNA

indent :: String -> String
indent = unlines . map ("  " ++) . lines

toFragmentShader :: RNA -> String
toFragmentShader rna = conclude $ go rna 0
  where
    go (Op0 o)          = op0GLSL o
    go (Op1 o i1)       = op1GLSL o (go i1)
    go (Op2 o i1 i2)    = op2GLSL o (go i1) (go i2)
    go (Op3 o i1 i2 i3) = op3GLSL o (go i1) (go i2) (go i3)

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

op0GLSL :: Op0 -> GLSLGen
op0GLSL (Solid col) n = (,n) $
    printf "vec3 col%d = vec3(%f,%f,%f);\n" n r g b
  where RGB r g b = toSRGB col
op0GLSL Gradient n = (,n) $
    printf "vec3 col%d = length(pos%d) * vec3(1.0,1.0,1.0);\n" n n

op1GLSL :: Op1 -> GLSLGen -> GLSLGen
op1GLSL Inv i1 n = (,n1) $ unlines
    [ printf "vec2 pos%d = ((1.0-length(pos%d))/length(pos%d)) * pos%d;" (n+1) n n n
    , src1
    ]
  where
    (src1, n1) = i1 (n+1)
op1GLSL (Swirl x) i1 n = (,n1) $ unlines
    [ printf "vec2 pos%d = length(pos%d) * vec2(cos(atan(pos%d.x, pos%d.y) + (1.0-length(pos%d))*%f),sin(atan(pos%d.x, pos%d.y) + (1.0-length(pos%d))*%f));" (n+1) n n n n x n n n x
    , src1
    ]
  where
    (src1, n1) = i1 (n+1)

op2GLSL :: Op2 -> GLSLGen -> GLSLGen -> GLSLGen
op2GLSL Before i1 i2 n = (,n2+1) $ unlines
    [ printf "vec2 pos%d = pos%d/0.8;" (n+1) n
    , src1
    , printf "vec2 pos%d = pos%d;" (n1+1) n
    , src2
    , printf "vec3 col%d;" (n2+1)
    , printf "if (length(pos%d) < 0.8) {" n
    , printf "   col%d = col%d;" (n2+1) n1
    , printf "} else {"
    , printf "   col%d = col%d;" (n2+1) n2
    , printf "};"
    ]
  where
    (src1, n1) = i1 (n+1)
    (src2, n2) = i2 (n1+1)

op2GLSL (Rays r) i1 i2 n = (,n2+1) $ unlines
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
    (src1, n1) = i1 n
    (src2, n2) = i2 (n1+1)
op2GLSL Checker i1 i2 n = (,n2+1) $ unlines
    [ src1
    , printf "vec2 pos%d = pos%d;" (n1+1) n
    , src2
    , printf "vec2 tmp%d = 6.0*(1.0/sqrt(2.0)) * mat2(1.0,1.0,-1.0,1.0) * pos%d;" n n
    , printf "vec3 col%d;" (n2+1)
    , printf "if (mod(tmp%d.x, 2.0) < 1.0 != mod(tmp%d.y, 2.0) < 1.0) {" n n
    , printf "   col%d = col%d;" (n2+1) n1
    , printf "} else {"
    , printf "   col%d = col%d;" (n2+1) n2
    , printf "};"
    ]
  where
    (src1, n1) = i1 n
    (src2, n2) = i2 (n1+1)

op3GLSL :: Op3 -> GLSLGen -> GLSLGen -> GLSLGen -> GLSLGen
op3GLSL Blur i1 i2 i3 n = (,n3+1) $ unlines
    [ src1
    , printf "vec2 pos%d = pos%d;" (n1+1) n
    , src2
    , printf "vec2 pos%d = pos%d;" (n2+1) n
    , src3
    , printf "vec3 col%d = length(col%d)/length(vec3(1.0,1.0,1.0)) * col%d + (1.0-length(col%d)/length(vec3(1.0,1.0,1.0))) * col%d;" (n3+1) n1 n2 n1 n3
    ]
  where
    (src1, n1) = i1 n
    (src2, n2) = i2 (n1+1)
    (src3, n3) = i3 (n2+1)
