{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module GLSL
    ( toFragmentShader
    , blankShader
    ) where

import Prelude hiding (unlines)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder
import Data.Colour.SRGB
import RNA
import Control.Monad.State.Strict

toFragmentShader :: RNA -> T.Text
toFragmentShader rna = runG $ conclude $ highlight $ go rna

-- The name of a variable
type GLVar = Builder
-- A GLSL expression
type GLExpr = Builder

-- A monad for writing code and generating fresh names
type G = State (Int, Builder)
runG :: G () -> T.Text
runG g = TL.toStrict $ toLazyText $ snd $ execState g (0, "")
l :: Builder -> G ()
l code = modify $ \(n,c) -> (n, c <> code <> "\n")

fresh :: Builder -> G Builder
fresh prefix = do
   n <- state $ \(n,c) -> (n, (n+1,c))
   return $ prefix <> build n

fl :: Builder -> GLExpr -> G GLVar
fl prefix code = do
    n <- fresh prefix
    l $ "float " <> n <> " = " <> code <> ";"
    return n
vec2 :: Builder -> GLExpr -> G GLVar
vec2 prefix code = do
    n <- fresh prefix
    l $ "vec2 " <> n <> " = " <> code <> ";"
    return n
vec3 :: Builder -> GLExpr -> G GLVar
vec3 prefix code = do
    n <- fresh prefix
    l $ "vec3 " <> n <> " = " <> code <> ";"
    return n

type Pattern = GLVar -> GLVar -> G GLVar

conclude :: Pattern -> G ()
conclude pat = do
  l "#define M_PI 3.1415926535897932384626433832795"
  l "uniform float u_extraData[5];"
  l "varying vec2 vDrawCoord;"
  l "void main() {"
  l "  float extraData = u_extraData[0];"
  l "  float anim = u_extraData[4];"
  l "  if (length(vDrawCoord) > 1.0) { gl_FragColor = vec4(0.0,0.0,0.0,0.0); return; }"
  col <- pat "vDrawCoord" "anim"
  l "  if (extraData > 2.5) {" -- extraData == 3: grey out
  l $ "  " <> col <> " = vec3(dot(vec3(0.299, 0.587, 0.114), " <> col <> "));"
  l "  }"
  l $ "  gl_FragColor = vec4(" <> col <> ", 1.0);"
  l "}"

highlight :: Pattern -> Pattern
highlight pat pos0 anim = do
  pos1 <- vec2 "pos" pos0
  l "  if (extraData > 0.5) {" -- need a hightlighting border
  l $ "    if (length(" <> pos0 <> ") > 0.9) {"
  l "      if (extraData > 1.5 && extraData < 2.5) {"
  l "        gl_FragColor = vec4(0,0,1.0,1.0);"
  l "      } else {"
  l "        gl_FragColor = vec4(0,0,0,0.0);"
  l "      };"
  l "      return;"
  l "    }"
  l $ "    " <> pos1 <> " = " <> pos0 <> " / 0.9;"
  l "    }"
  pat pos1 anim

blankShader :: T.Text
blankShader = runG $ conclude $ go (Solid (RGB 1 1 1))

phase, len :: Builder -> Builder
phase x = "atan(" <> x <> ".x, " <> x <> ".y)"
len x = "length(" <> x <> ")"
mkPolar :: Builder -> Builder -> Builder
mkPolar mag x = "(" <> mag <> " * vec2(cos(" <> x <> "),sin(" <> x <> ")))"
float :: Int -> Builder
float x = build (fromIntegral x :: Double)
ifThenElse :: GLExpr -> GLExpr -> GLExpr -> GLExpr
ifThenElse s t e = "( " <> s <> " ? " <> t <> " : " <> e <> " )"
tween :: GLExpr -> GLExpr -> GLExpr -> GLExpr
tween x a b = "mix(" <> a <> "," <> b <> "," <> x <> ")"

anim0 :: GLExpr -> G GLExpr
anim0 anim = fl "anim" $ "smoothstep(0.0,1.0," <> anim <> ")"

animHalf :: GLExpr -> G (GLExpr, GLExpr)
animHalf anim = do
    x <- fl "sub1" $ "clamp(" <> anim <> " * 2.0, 0.0, 1.0)"
    y <- fl "sub2" $ "clamp(" <> anim <> " * 2.0 - 1.0, 0.0, 1.0)"
    return (x,y)

animUnary :: GLExpr -> G (GLExpr, GLExpr)
animUnary anim = do
    (x,y) <- animHalf anim
    y' <- anim0 y
    return (x,y')

animBinary :: GLExpr -> G (GLExpr, GLExpr, GLExpr)
animBinary anim = do
    x <- fl "sub1" $ "clamp(" <> anim <> " * 3.0, 0.0, 1.0)"
    y <- fl "sub2" $ "clamp(" <> anim <> " * 3.0 - 1.0, 0.0, 1.0)"
    z <- fl "sub3" $ "clamp(" <> anim <> " * 3.0 - 2.0, 0.0, 1.0)"
    y' <- anim0 y
    return (x,y',z)

go :: RNA -> Pattern

go (Solid color) = \_pos anim -> do
    a <- anim0 anim
    vec3 "col" $ tween a
        "vec3(1.0,1.0,1.0)"
        ("vec3(" <> build r <> "," <> build g <> "," <> build b <> ")")
  where RGB r g b = color

go (Blend x r1 r2) = \pos anim -> do
    (al, a, ar) <- animBinary anim
    col1 <- go r1 pos al
    col2 <- go r2 pos ar
    vec3 "col" $ tween (a <> " * " <> build x) col2 col1

go (Checker x r1 r2) = \pos anim -> do
    (al, a, ar) <- animBinary anim
    col1 <- go r1 pos al
    col2 <- go r2 pos ar
    tmp <- vec2 "tmp" $ build x <> "* (vec2(1.0,0.0) + " <> pos <> ")"
    vec3 "col" $ ifThenElse
        ("abs(mod(" <> tmp <> ".x + 1.0, 2.0) - 1.0) + abs(mod(" <> tmp <> ".y, 2.0) - 1.0) < " <> a)
        col2 col1

go (Rotate x r1) = \pos anim -> do
    (al, a) <- animUnary anim
    p' <- fl "phase" $ phase pos <> " + " <> a <> " * " <> build x
    pos' <- vec2 "pos" $ mkPolar (len pos) p'
    go r1 pos' al

go (Invert r1) = \pos anim -> do
    (al, a) <- animUnary anim
    mag <- fl "len" $ len pos
    -- simple
    -- mag' <- fl "newlen" $ tween a mag ("1.0 - " <> mag)
    -- more complex
    mag' <- fl "newlen" $ ifThenElse
        (a <> "< 0.5")
        (tween ("clamp(" <> a <> " * 2.0, 0.0, 1.0)")
          mag
          ("1.0 - (2.0*" <> mag <> " - 1.0)*(2.0*" <> mag <> " - 1.0)"))
        (tween ("clamp(" <> a <> " * 2.0 - 1.0, 0.0, 1.0)")
          ("1.0 - (2.0*" <> mag <> " - 1.0)*(2.0*" <> mag <> " - 1.0)")
          ("1.0 - " <> mag))
    pos' <- vec2 "pos" $ "(" <> mag' <> "/" <> mag <> ") * " <> pos
    go r1 pos' al

go (Swirl x r1) = \pos anim -> do
    (al, a) <- animUnary anim
    mag <- fl "len" $ len pos
    p' <- fl "phase" $ phase pos <> " + " <> a <> " * (1.0 - " <> mag <> ") * " <> build x
    pos' <- vec2 "pos" $ mkPolar mag p'
    go r1 pos' al

go (Dilated r r1) = \pos anim -> do
    (al, a) <- animUnary anim
    p <- fl "phase" $ phase pos
    mag <- fl "len" $ len pos
    p' <- fl "phase" $ p <> " + " <> a <> " * 1.0/" <> float r <> " * sin(" <> p <> " * " <> float r <> ")"
    pos' <- vec2 "pos" $ mkPolar mag p'
    go r1 pos' al

go (Rays r r1 r2) = \pos anim -> do
    (al, a, ar) <- animBinary anim
    col1 <- go r1 pos al
    col2 <- go r2 pos ar
    vec3 "col" $ ifThenElse
        ("mod( " <> phase pos <> "/" <> build (pi :: Double) <> " * " <> float r <> " + 0.5 * " <> a <> ", 2.0) < " <> a)
        col2 col1

go (Gradient r1 r2) = \pos anim -> do
    (al, ar) <- animHalf anim
    col1 <- go r1 pos al
    col2 <- go r2 pos ar
    vec3 "col" $ tween (len pos) col1 col2

go (Ontop x r1 r2) = \pos anim -> do
    (al, a, ar) <- animBinary anim
    pos1 <- vec2 "pos" $ pos <> "/" <> build x
    col1 <- go r1 pos1 ar
    col2 <- go r2 pos al
    vec3 "col" $ ifThenElse
        (len pos <> " < " <> a <> " * " <> build x)
        col1 col2

-- The following is borrowed from the formatting library,
-- which depends on Clock, which cannot be built on android easily

class Buildable p where
    build :: p -> Builder

instance Buildable String where
    build = fromString
    {-# INLINE build #-}

instance Buildable Int where
    build = build . show
    {-# INLINE build #-}

instance Buildable Double where
    build = build . show
    {-# INLINE build #-}
