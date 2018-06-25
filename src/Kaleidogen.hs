{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Kaleidogen ( main ) where

import Data.Colour.Names
import Data.Maybe
import Text.Read
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar
import Control.Lens ((^.))
import Language.Javascript.JSaddle hiding ((!!))
import Language.Javascript.JSaddle.Warp (run)

import RNA
import GLSL
import Expression


paintGL :: JSVal -> (JSVal -> JSM ()) -> String -> JSM ()
paintGL canvas printErr fragmentShaderSource = do
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

main :: IO ()
main = run 3709 $ do
    doc <- jsg "document"
    -- doc ^. js "body" ^. js "style" ^. jss "margin" "0"
    -- doc ^. js "body" ^. js "style" ^. jss "padding" "0"

    title <- doc ^. js "head" ^. js1 "getElementsByTagName" "title" ^.js1 "item" (0::Int)
    jsg "console" ^. js1 "log" title
    title ^. jss "innerHTML" "Kaleidogen"

    topDiv <- doc ^. js1 "createElement" "div"
    topDiv ^. jss "align" "center"
    doc ^. js "body" ^. js1 "appendChild" topDiv

    canvas <- doc ^. js1 "createElement" "canvas"
    canvas ^. js "style" ^. jss "border" "1px solid black"
    canvas ^. js "style" ^. jss "width" "30%"
    canvas ^. jss "width" "300"
    canvas ^. jss "height" "300"
    topDiv ^. js1 "appendChild" canvas
    topDiv ^. js1 "appendChild" (doc ^. js1 "createElement" "br")

    input <- doc ^. js1 "createElement" "input"
    input ^. js "style" ^. jss "width" "30%"
    topDiv ^. js1 "appendChild" input

    debug1Div <- doc ^. js1 "createElement" "div"
    debug1Div ^. js "style" ^. jss "width" "80%"
    debug1Div ^. js "style" ^. jss "text-align" "left"
    debug1Div ^. js "style" ^. jss "font-family" "mono"
    debug1Div ^. js "style" ^. jss "white-space" "pre"
    topDiv ^. js1 "appendChild" debug1Div

    debug2Div <- doc ^. js1 "createElement" "div"
    debug2Div ^. js "style" ^. jss "width" "80%"
    debug2Div ^. js "style" ^. jss "text-align" "left"
    debug2Div ^. js "style" ^. jss "font-family" "mono"
    debug2Div ^. js "style" ^. jss "white-space" "pre"
    topDiv ^. js1 "appendChild" debug2Div

    paintGL canvas (const (return ())) $ toFragmentShader $ Op0 (Solid red)

    let update = do
            Just userInput <- input ^. js "value" >>= fromJSVal
            -- jsg "console" ^. js1 "log" ("paintGenome", userInput)
            let nums = mapMaybe readMaybe (words userInput)
            let i = toFragmentShader $ runProgram nums
            debug1Div ^. jss "innerHTML" i
            paintGL canvas (\s -> debug2Div ^. jss "innerHTML" s)
                           i

    {-
    ctx <- askJSM
    liftIO . forkIO $ forever $ (`runJSM` ctx) $
            liftIO (takeMVar inputMVar) >>= paintGenome canvas
    -}

    input ^. jss "onchange" (fun $ \_ _ _ -> update)
    input ^. jss "onkeyup" (fun $ \_ _ _ -> update)

    exitMVar <- liftIO newEmptyMVar

    -- Force all all the lazy evaluation to be executed
    syncPoint

    -- In GHC compiled version the WebSocket connection will end when this
    -- thread ends.  So we will wait until the exitMVar is filled
    liftIO $ takeMVar exitMVar
    return ()



