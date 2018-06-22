{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Kaleidogen ( main ) where

import Debug.Trace
import Data.Word
import Data.Colour
import Data.Colour.CIE
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Complex
import Data.Maybe
import Text.Read
import Text.Printf
import Data.Monoid ((<>))
import Data.Foldable
import qualified Data.ByteString as BS
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Lens ((^.))
import Language.Javascript.JSaddle hiding ((!!))
-- import qualified GHCJS.Buffer as B
import Language.Javascript.JSaddle.Helper

-- Function evaluator

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

baseColor :: Int -> Colour Double
baseColor n = colors !! n'
  where
    n' = n `mod` length colors
    colors = [ black, white
             , red, green, blue
             , cyan, magenta, yellow
             ]

-- GLSL evaluator

type GLSLGen = Integer -> (String, Integer)

toGLSL :: RNA -> String
toGLSL rna = conclude $ go rna 0
  where
    go (Op0 o)          = op0GLSL o
    go (Op1 o i1)       = op1GLSL o (go i1)
    go (Op2 o i1 i2)    = op2GLSL o (go i1) (go i2)
    go (Op3 o i1 i2 i3) = op3GLSL o (go i1) (go i2) (toImg i3)

conclude :: (String, Integer) -> String
conclude (pgm, r) = pgm ++ "gl_FragColor = vec4(col" ++ show r ++ ", 1.0);\n"

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

op2GLSL (Rays r) i1 i2 n = i1 n
op2GLSL Checker i1 i2 n = i1 n

op3GLSL o i1 i2 i3 = i1

{-
op1Img Inv i = \c -> i $
    let (m,p) = polar c in
    if m > 1 then c else mkPolar (1 - m) p
op1Img (Swirl x) i = \c -> i $
    let (m,p) = polar c in
    mkPolar m (p + (1-m) * x)

op2Img Before fb bg = mix ((<0.8) . magnitude) (scale fb) bg
op2Img (Rays n) i1 i2 = mix (\c -> even (round (phase c / pi * fromIntegral n))) i1 i2
op2Img Checker i1 i2 = mix f i1 i2
  where f c = let c' = c * cis (pi/4) * 6
              in even (round (realPart c') + round (imagPart c'))


op3Img Blur i1 i2 i3 = \c ->
    blend (brightness (i1 c)) (i2 c) (i3 c)
-}

type Program = [Int]
type Inst = Int

data Op0 = Solid (Colour Double)
         | Gradient

data Op1 = Inv
         | Swirl Double

data Op2 = Before
         | Checker
         | Rays Int

data Op3 = Blur

data RNA
    = Op0 Op0
    | Op1 Op1 RNA
    | Op2 Op2 RNA RNA
    | Op3 Op3 RNA RNA RNA

type Stack = [RNA]

runInst :: Inst -> Stack -> Stack
runInst 0 s            = Op0 Gradient : s
runInst 1 (i2:i1:s)    = Op2 Before i1 i2 : s
runInst 2 (i3:i2:i1:s) = Op3 Blur i3 i1 i2  : s
runInst 3 (i2:i1:s)    = Op2 Checker i1 i2  : s
runInst 4 (i:s)        = Op1 Inv i          : s
runInst c (i2:i1:s)    | c >= 10 && c < 17  = Op2 (Rays (c - 9)) i1 i2 : s
runInst c (i:s)        | c >= 20 && c <= 30 = Op1 (Swirl (fromIntegral (c - 25))) i : s
-- fallback
runInst n s  = Op0 (Solid (baseColor n)) : s

runProgram :: Program -> RNA
runProgram = collapsStack . foldl' (flip runInst) []

collapsStack :: Stack -> RNA
collapsStack [] = Op0 (Solid black)
collapsStack [i] = i
collapsStack (i:is) = Op2 Before fg bg
  where
    bg = i
    fg = collapsStack is

rainbow :: Double -> Img
rainbow t c | r > 1 = black
            | otherwise = sRGB24
                          (toW $ sin t')
                          (toW $ sin (t' + 2*pi/3))
                          (toW $ sin (t' + 4*pi/3))
  where r = magnitude c
        t' = t + r
        toW x = round (max 0 $ min 255 $ 256 * ((x + 1) / 2))

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
    {-
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
    -}
    return ()

animate :: JSVal -> JSM ()
animate canvas = do
    paint canvas (rainbow 0)
    jsCtx <- askJSM
    liftIO . forkIO . forever $
        (`runJSM` jsCtx) . nextAnimationFrame $ \ t -> do
            jsg "console" ^. js1 "log" t
            paint canvas (rainbow t)
            -- paint canvas (\_ _ -> (100,100,100))
            jsg "console" ^. js1 "log" "hi"
            return ()
    return ()


vertexShaderSource =
  "attribute vec2 a_position;\
  \void main() {\
  \  gl_Position = vec4(a_position, 0, 1);\
  \}"

fragmentShaderSource_prefix = unlines
  [ "precision mediump float;"
  , "uniform vec2 u_windowSize;"
  , "void main() {"
  , "  float s = 2.0 / min(u_windowSize.x, u_windowSize.y);"
  , "  vec2 pos0 = s * (gl_FragCoord.xy - 0.5 * u_windowSize);"
  , "  if (length(pos0) > 1.0) { gl_FragColor = vec4(0,0,0,0); return; }"
  ]
indent = unlines . map ("  " ++) . lines
fragmentShaderSource_default = unlines
  [ "  gl_FragColor = vec4(pos0,0,1);"
  ]
fragmentShaderSource_suffix = unlines
  [ "}"
  ]

paintGL :: JSVal -> (JSVal -> JSM ()) -> (JSVal -> JSM ()) -> String -> JSM ()
paintGL canvas printSrc printErr fragmentShaderSource_infix = do
  -- adaption of
  -- https://blog.mayflower.de/4584-Playing-around-with-pixel-shaders-in-WebGL.html
  gl <- canvas ^. js1 "getContext" "experimental-webgl"
  gl ^. jsf "viewport" (0::Int, 0::Int, gl ^. js "drawingBufferWidth", gl ^. js "drawingBufferHeight")

  -- gl ^. jsf "clearColor" [1.0, 0.0, 0.0, 1.0 :: Double]
  -- gl ^. js1 "clear" (gl^. js "COLOR_BUFFER_BIT")

  buffer <- gl ^. js0 "createBuffer"
  gl ^. jsf "bindBuffer" (gl ^. js "ARRAY_BUFFER", buffer)
  gl ^. jsf "bufferData"
    ( gl ^. js "ARRAY_BUFFER"
    , new (jsg "Float32Array") [[
      -1.0, -1.0,
       1.0, -1.0,
      -1.0,  1.0,
      -1.0,  1.0,
       1.0, -1.0,
       1.0,  1.0 :: Double]]
    ,  gl ^. js "STATIC_DRAW"
    )
  -- jsg "console" ^. js1 "log" (gl ^. js0 "getError")

  vertexShader <- gl ^. js1 "createShader" (gl ^. js "VERTEX_SHADER")
  gl ^. js2 "shaderSource" vertexShader vertexShaderSource
  gl ^. js1 "compileShader" vertexShader
  jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" vertexShader)

  fragmentShader <- gl ^. js1 "createShader" (gl ^. js "FRAGMENT_SHADER")
  let fragmentShaderSource = fragmentShaderSource_prefix ++
                             indent fragmentShaderSource_infix ++
                             fragmentShaderSource_suffix
  toJSVal fragmentShaderSource >>= printSrc
  gl ^. js2 "shaderSource" fragmentShader fragmentShaderSource
  gl ^. js1 "compileShader" fragmentShader
  jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" fragmentShader)
  gl ^. js1 "getShaderInfoLog" fragmentShader >>= printErr

  program <- gl ^. js0 "createProgram"
  gl ^. js2 "attachShader" program vertexShader
  gl ^. js2 "attachShader" program fragmentShader
  gl ^. js1 "linkProgram" program
  gl ^. js1 "useProgram" program
  jsg "console" ^. js1 "log" (gl ^. js1 "getProgramInfoLog" program)

  positionLocation <- gl ^. js2 "getAttribLocation" program "a_position"
  gl ^. js1 "enableVertexAttribArray" positionLocation
  gl ^. jsf "vertexAttribPointer" (positionLocation, 2::Int, gl ^. js "FLOAT", False, 0::Int, 0::Int)
  jsg "console" ^. js1 "log" program

  windowSizeLocation <- gl ^. js2 "getUniformLocation" program "u_windowSize"
  gl ^. jsf "uniform2f"( windowSizeLocation, gl ^. js "drawingBufferWidth", gl ^. js "drawingBufferHeight")

  gl ^. jsf "drawArrays" (gl ^. js "TRIANGLES", 0::Int, 6::Int);
  return ()



main = do
    doc <- jsg "document"
    -- doc ^. js "body" ^. js "style" ^. jss "margin" "0"
    -- doc ^. js "body" ^. js "style" ^. jss "padding" "0"

    div <- doc ^. js1 "createElement" "div"
    div ^. jss "align" "center"
    doc ^. js "body" ^. js1 "appendChild" div

    canvas <- doc ^. js1 "createElement" "canvas"
    canvas ^. js "style" ^. jss "border" "1px solid black"
    canvas ^. js "style" ^. jss "width" "30%"
    canvas ^. jss "width" "300"
    canvas ^. jss "height" "300"
    div ^. js1 "appendChild" canvas
    div ^. js1 "appendChild" (doc ^. js1 "createElement" "br")

    input <- doc ^. js1 "createElement" "input"
    input ^. js "style" ^. jss "width" "30%"
    div ^. js1 "appendChild" input

    debug1Div <- doc ^. js1 "createElement" "div"
    debug1Div ^. js "style" ^. jss "width" "80%"
    debug1Div ^. js "style" ^. jss "text-align" "left"
    debug1Div ^. js "style" ^. jss "font-family" "mono"
    debug1Div ^. js "style" ^. jss "white-space" "pre"
    div ^. js1 "appendChild" debug1Div

    debug2Div <- doc ^. js1 "createElement" "div"
    debug2Div ^. js "style" ^. jss "width" "80%"
    debug2Div ^. js "style" ^. jss "text-align" "left"
    debug2Div ^. js "style" ^. jss "font-family" "mono"
    debug2Div ^. js "style" ^. jss "white-space" "pre"
    div ^. js1 "appendChild" debug2Div

    -- paint canvas (rainbow 0)
    paintGL canvas (const (return ())) (const (return ())) (toGLSL $ Op0 (Solid red))

    inputMVar <- liftIO newEmptyMVar
    let update = do
            Just val <- input ^. js "value" >>= fromJSVal
            -- jsg "console" ^. js1 "log" ("paintGenome", val)
            let nums = mapMaybe readMaybe (words val)
            let i = toGLSL $ runProgram nums
            paintGL canvas (\s -> debug1Div ^. jss "innerHTML" s)
                           (\s -> debug2Div ^. jss "innerHTML" s)
                           i

    {-
    ctx <- askJSM
    liftIO . forkIO $ forever $ (`runJSM` ctx) $
            liftIO (takeMVar inputMVar) >>= paintGenome canvas
    -}

    input ^. jss "onchange" (fun $ \_ _ _ -> update)
    input ^. jss "onkeyup" (fun $ \_ _ _ -> update)

{-
    -- Create a haskell function call back for the onclick event
    doc ^. jss "onclick" (fun $ \ _ _ [e] -> do
        x <- e ^. js "clientX" >>= valToNumber
        y <- e ^. js "clientY" >>= valToNumber
        newParagraph <- doc ^. js1 "createElement" "p"
        newParagraph ^. js1 "appendChild" (
            doc ^. js1 "createTextNode" ("Click " ++ show (x, y)))
        doc ^. js "body" ^. js1 "appendChild" newParagraph
        return ())

    -- Make an exit button
    exit <- doc ^. js1 "createElement" "span"
    exit ^. js1 "appendChild" (
        doc ^. js1 "createTextNode" "Click here to exit")
    doc ^. js "body" ^. js1 "appendChild" exit
    exit ^. jss "onclick" (fun $ \ _ _ _ -> liftIO $ putMVar exitMVar ())
-}

    exitMVar <- liftIO newEmptyMVar

    -- Force all all the lazy evaluation to be executed
    syncPoint

    -- Must come after the syncPoint
    -- animate canvas

    -- Animate the color of the exit button
    {-
    ctx <- askJSM
    liftIO . forkIO . forever $
        (`runJSM` ctx) . nextAnimationFrame $ \ t -> do
            let n = floor ((sin (3 * t) + 1) * 128)
                (h1, h2) = n `divMod` 16
                hexDigits = ['0'..'9'] <> ['A'..'F']
            exit ^. js "style" ^. jss "color" ("#0000" <> [hexDigits !! h1, hexDigits !! h2])
            -- paint canvas (rainbow (t * 60))
            return ()
    -}

    -- In GHC compiled version the WebSocket connection will end when this
    -- thread ends.  So we will wait until the user clicks exit.
    liftIO $ takeMVar exitMVar
    -- doc ^. js "body" ^. jss "innerHTML" "<h1>Ka kite ano (See you later)</h1>"
    return ()



