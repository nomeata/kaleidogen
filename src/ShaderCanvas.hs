{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module ShaderCanvas where

import Reflex.Dom

import Data.Maybe
import Data.Text as Text (Text, unlines)
import Control.Monad.IO.Class
import Control.Monad.Fix
import Data.Foldable

import GHCJS.DOM
import GHCJS.DOM.Types hiding (Text, Event)
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.WebGLRenderingContextBase
import GHCJS.DOM.Window
import GHCJS.DOM.Element
import GHCJS.DOM.RequestAnimationFrameCallback
-- import GHCJS.DOM.EventM (on, preventDefault)
import qualified GHCJS.DOM.EventTargetClosures as DOM (EventName, unsafeEventName)

import Language.Javascript.JSaddle.Object hiding (array)
-- import Control.Lens ((^.))


vertexShaderSource :: Text
vertexShaderSource = Text.unlines
  [ "attribute vec2 a_position;"
  , "uniform vec2 u_windowSize;"
  , "uniform float u_size;"
  , "uniform vec2 u_pos;"
  , "varying vec2 vDrawCoord;"
  , "void main() {"
  , "  vDrawCoord = vec2(a_position); "
  , "  vec2 pos = vec2(1.0,-1.0) * (2.0 * (u_size * a_position + u_pos)/u_windowSize - vec2(1,1));"
  , "  gl_Position = vec4(pos, 0, 1);"
  , "}"
  ]

-- | An example fragment shader program, drawing a red circle
trivialFragmentShader :: Text
trivialFragmentShader = Text.unlines
  [ "precision mediump float;"
  , "varying vec2 vDrawCoord;"
  , "void main() {"
  , "  vec2 pos = vDrawCoord;"
  , "  // pos is a scaled pixel position, (0,0) is in the center of the canvas"
  , "  // If the position is outside the inscribed circle, make it transparent"
  , "  if (length(pos) > 1.0) { gl_FragColor = vec4(0,0,0,0); return; }"
  , "  // Otherwise, return red"
  , "  gl_FragColor = vec4(1.0,0.0,0.0,1.0);"
  , "}"
  ]

type Drawable = (Text, Double, (Double, Double), Double)

paintGL :: MonadDOM m => (Text -> m ()) -> [Drawable] -> HTMLCanvasElement -> m ()
paintGL printErr toDraw canvas =
  -- adaption of
  -- https://blog.mayflower.de/4584-Playing-around-with-pixel-shaders-in-WebGL.html

  getContext canvas ("experimental-webgl"::Text) ([]::[()]) >>= \case
    Nothing ->
      -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" vertexShader)
      return ()
    Just gl' -> do
      gl <- unsafeCastTo WebGLRenderingContext gl'

      w <- getDrawingBufferWidth gl
      h <- getDrawingBufferHeight gl
      viewport gl 0 0 w h

      clearColor gl 1 1 1 1
      clear gl COLOR_BUFFER_BIT

      enable gl BLEND
      blendFunc gl ONE ONE_MINUS_SRC_ALPHA

      buffer <- createBuffer gl
      bindBuffer gl ARRAY_BUFFER (Just buffer)
      array <- liftDOM (new (jsg ("Float32Array"::Text))
            [[ -1.0, -1.0,
                1.0, -1.0,
               -1.0,  1.0,
               -1.0,  1.0,
                1.0, -1.0,
                1.0,  1.0 :: Double]])
        >>= unsafeCastTo Float32Array
      let array' = uncheckedCastTo ArrayBuffer array
      bufferData gl ARRAY_BUFFER (Just array') STATIC_DRAW

      for_ toDraw $ \(fragmentShaderSource, extraData, (x,y), size) -> do
          vertexShader <- createShader gl VERTEX_SHADER
          shaderSource gl (Just vertexShader) vertexShaderSource
          compileShader gl (Just vertexShader)
          -- _ <- liftJSM $ jsg (T.pack "console") ^. js1 (T.pack "log") (gl ^. js1 (T.pack "getShaderInfoLog") vertexShader)

          fragmentShader <- createShader gl FRAGMENT_SHADER
          shaderSource gl (Just fragmentShader) fragmentShaderSource
          compileShader gl (Just fragmentShader)
          -- _ <- liftJSM $ jsg (T.pack "console") ^. js1 (T.pack "log") (gl ^. js1 (T.pack "getShaderInfoLog") fragmentShader)
          err <- getShaderInfoLog gl (Just fragmentShader)
          printErr (fromMaybe "" err)

          program <- createProgram gl
          attachShader gl (Just program) (Just vertexShader)
          attachShader gl (Just program) (Just fragmentShader)
          linkProgram gl (Just program)
          useProgram gl (Just program)
          -- _ <- liftJSM $ jsg (T.pack "console") ^. js1 (T.pack "log") (gl ^. js1 (T.pack "getProgramInfoLog") program)

          positionLocation <- getAttribLocation gl (Just program) ("a_position" :: Text)
          enableVertexAttribArray gl (fromIntegral positionLocation)
          vertexAttribPointer gl (fromIntegral positionLocation) 2 FLOAT False 0 0
          -- liftJSM $ jsg ("console"::Text) ^. js1 ("log"::Text) program

          windowSizeLocation <- getUniformLocation gl (Just program) ("u_windowSize" :: Text)
          uniform2f gl (Just windowSizeLocation) (fromIntegral w) (fromIntegral h)

          posLocation <- getUniformLocation gl (Just program) ("u_pos" :: Text)
          uniform2f gl (Just posLocation) x y

          sizeLocation <- getUniformLocation gl (Just program) ("u_size" :: Text)
          uniform1f gl (Just sizeLocation) size

          extraDataLocation <- getUniformLocation gl (Just program) ("u_extraData" :: Text)
          uniform1f gl (Just extraDataLocation) extraData

          drawArrays gl TRIANGLES 0 6

webglcontextrestored :: DOM.EventName HTMLCanvasElement WebGLContextEvent
webglcontextrestored = DOM.unsafeEventName "webglcontextrestored"

webglcontextlost :: DOM.EventName HTMLCanvasElement WebGLContextEvent
webglcontextlost = DOM.unsafeEventName "webglcontextlost"


querySize :: (IsElement self, MonadJSM m) => self -> m (Double, Double)
querySize domEl = liftJSM $ do
      w <- getClientWidth domEl
      h <- getClientHeight domEl
      Just win <- currentWindow
      r <- getDevicePixelRatio win
      return (r * w, r * h)

autoResizeCanvas ::
    (MonadFix m, MonadHold t m,
    MonadJSM (Performable m), MonadJSM m, PerformEvent t m,
    TriggerEvent t m) =>
    HTMLCanvasElement -> m (Dynamic t (Double, Double), Event t ())
autoResizeCanvas domEl =  do
  -- Automatically update the size when it changes
  animationFrameE <- getAnimationFrameE
  initialSize <- querySize domEl
  eCanvasSize <- performEvent (querySize domEl <$ animationFrameE)
  dCanvasSize <- holdUniqDyn =<< holdDyn initialSize eCanvasSize
  eResized <- performEvent $ (<$> updated dCanvasSize) $ \(w,h) -> do
    setWidth domEl (ceiling w)
    setHeight domEl (ceiling h)
    -- liftJSM $ jsg ("console"::Text) ^. jsf ("log"::Text) ("resize" :: Text, w, h)
    return ()
  return (dCanvasSize, eResized)

shaderCanvas ::
    (MonadWidget t m) =>
    Dynamic t [Drawable] ->
    m (Event t (Int,Int), Dynamic t Text, Dynamic t (Double, Double))
shaderCanvas toDraw
    = snd <$> shaderCanvas' toDraw

shaderCanvas' ::
    (MonadWidget t m) =>
    Dynamic t [Drawable] ->
    m (El t, (Event t (Int, Int), Dynamic t Text, Dynamic t (Double, Double)))
shaderCanvas' toDraw = do
  (canvasEl, _) <- el' "canvas" blank
  (eError, reportError) <- newTriggerEvent
  pb <- getPostBuild

  let eClick = domEvent Mousedown canvasEl

  domEl <- unsafeCastTo HTMLCanvasElement $ _element_raw canvasEl
  (dCanvasSize, eResized) <- autoResizeCanvas domEl

  let eDraw = leftmost
        [ tag (current toDraw) eResized
        , updated toDraw
        , tag (current toDraw) pb
        ]

  _ <- performEvent $ (<$> eDraw) $ \src ->
    paintGL (liftIO . reportError) src domEl

  dErr <- holdDyn "" eError
  return (canvasEl, (eClick, dErr, dCanvasSize))


getAnimationFrameE :: (TriggerEvent t m, MonadJSM m) => m (Event t Double)
getAnimationFrameE = mdo
  (ev, trigger) <- newTriggerEvent
  Just win <- currentWindow
  let go d = do
      liftIO $ trigger d
      cb <- newRequestAnimationFrameCallback go
      _ <- requestAnimationFrame win cb
      return ()
  liftJSM $ go 0.0
  return ev

getAnimationFrameD :: (Reflex t, MonadHold t m, MonadFix m, TriggerEvent t m, MonadJSM m) => m (Dynamic t Double)
getAnimationFrameD = holdDyn 0 =<< getAnimationFrameE
