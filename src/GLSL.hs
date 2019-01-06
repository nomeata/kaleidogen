{-# LANGUAGE OverloadedStrings #-}
module GLSL (toFragmentShader, blankShader) where

import Prelude hiding (unlines)
import Data.Monoid
import Formatting.Buildable
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

type Pattern = GLVar -> G GLVar

conclude :: Pattern -> G ()
conclude pat = do
  l "uniform vec4 u_extraData;"
  l "varying vec2 vDrawCoord;"
  l "void main() {"
  l "  float extraData = u_extraData.x;"
  l "  if (length(vDrawCoord) > 1.0) { gl_FragColor = vec4(0,0,0,0.0); return; }"
  col <- pat "vDrawCoord"
  l $ "  gl_FragColor = vec4(" <> col <> ", 1.0);"
  l "}"

highlight :: Pattern -> Pattern
highlight pat pos0 = do
  pos1 <- vec2 "pos" pos0
  l "  if (extraData > 0.5) {" -- need a hightlighting border
  l $ "    if (length(" <> pos0 <> ") > 0.9) {"
  l "      if (extraData > 1.5) {"
  l "        gl_FragColor = vec4(0,0,1.0,1.0);"
  l "      } else {"
  l "        gl_FragColor = vec4(0,0,0,0.0);"
  l "      };"
  l "      return;"
  l "    }"
  l $ "    " <> pos1 <> " = " <> pos0 <> " / 0.9;"
  l "    }"
  pat pos1

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

go :: RNA -> Pattern

go (Solid color) = \_pos ->
    vec3 "col" $ "vec3(" <> build r <> "," <> build g <> "," <> build b <> ")"
  where RGB r g b = color

go (Blend x r1 r2) = \pos -> do
    col1 <- go r1 pos
    col2 <- go r2 pos
    vec3 "col" $ build x <> " * " <> col1 <> " + (1.0-" <> build x <> ") * " <> col2

go (Checker x r1 r2) = \pos -> do
    col1 <- go r1 pos
    col2 <- go r2 pos
    tmp <- vec2 "tmp" $ build x <> "*(1.0/sqrt(2.0)) * mat2(1.0,1.0,-1.0,1.0) * " <> pos
    vec3 "col" $ ifThenElse
        ("mod(" <> tmp <> ".x, 2.0) < 1.0 != mod(" <> tmp <> ".y, 2.0) < 1.0")
        col1 col2

go (Rotate x r1) = \pos -> do
    p' <- fl "phase" $ phase pos <> " + " <> build x
    pos' <- vec2 "pos" $ mkPolar (len pos) p'
    go r1 pos'

go (Invert r1) = \pos -> do
    pos' <- vec2 "pos" $ "((1.0-length(" <> pos <> "))/length(" <> pos <> ")) * " <> pos
    go r1 pos'

go (Swirl x r1) = \pos -> do
    mag <- fl "len" $ len pos
    p' <- fl "phase" $ phase pos <> " + (1.0 - " <> mag <> ") * " <> build x
    pos' <- vec2 "pos" $ mkPolar mag p'
    go r1 pos'

go (Dilated r r1) = \pos -> do
    p <- fl "phase" $ phase pos
    mag <- fl "len" $ len pos
    p' <- fl "phase" $ p <> " + 1.0/" <> float r <> " * sin(" <> p <> " * " <> float r <> ")"
    pos' <- vec2 "pos" $ mkPolar mag p'
    go r1 pos'

go (Rays r r1 r2) = \pos -> do
    col1 <- go r1 pos
    col2 <- go r2 pos
    vec3 "col" $ ifThenElse
        ("mod( " <> phase pos <> "/" <> build (pi :: Double) <> " * " <> float r <> ", 2.0) < 1.0")
        col1 col2

go (Gradient r1 r2) = \pos -> do
    col1 <- go r1 pos
    col2 <- go r2 pos
    tmp <- fl "tmp" $ len pos
    vec3 "col" $ tmp <> " * " <> col1 <> " + (1.0 - " <> tmp <> ") * " <> col2

go (Ontop x r1 r2) = \pos -> do
    pos1 <- vec2 "pos" $ pos <> "/" <> build x
    col1 <- go r1 pos1
    col2 <- go r2 pos
    vec3 "col" $ ifThenElse
        (len pos <> " < " <> build x)
        col1 col2
