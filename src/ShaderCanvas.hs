{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module ShaderCanvas
    ( CompiledProgram
    , trivialFragmentShader
    , shaderCanvas
    , shaderCanvas'
    , saveToPNG
    ) where

import CanvasSave

import Reflex.Dom

import Data.Maybe
import Data.Text as Text (Text, unlines)
import Control.Monad.Fix
import Data.Foldable
import Data.Bifunctor
import Data.Int
import Control.Monad
import Data.IORef
import Control.Monad.IO.Class

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M

import GHCJS.DOM
import GHCJS.DOM.Types hiding (Text, Event)
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.WebGLRenderingContextBase
import GHCJS.DOM.Window
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.EventM (mouseOffsetXY)
-- import GHCJS.DOM.EventM (on, preventDefault)

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

type CommonStuff = WebGLShader

commonSetup :: MonadJSM m => WebGLRenderingContext -> m CommonStuff
commonSetup gl = do
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

    vertexShader <- createShader gl VERTEX_SHADER
    shaderSource gl (Just vertexShader) vertexShaderSource
    compileShader gl (Just vertexShader)
    -- _ <- liftJSM $ jsg (T.pack "console") ^. js1 (T.pack "log") (gl ^. js1 (T.pack "getShaderInfoLog") vertexShader)

    return vertexShader

data CompiledProgram = CompiledProgram
    { compiledProgram :: WebGLProgram
    , positionLocation :: Int32
    , windowSizeLocation  :: WebGLUniformLocation
    , posLocation :: WebGLUniformLocation
    , sizeLocation :: WebGLUniformLocation
    , extraDataLocation  :: WebGLUniformLocation
    }

compileFragmentShader :: MonadJSM m => WebGLRenderingContext -> WebGLShader -> Text -> m CompiledProgram
compileFragmentShader gl vertexShader fragmentShaderSource = do
    fragmentShader <- createShader gl FRAGMENT_SHADER
    shaderSource gl (Just fragmentShader) fragmentShaderSource
    compileShader gl (Just fragmentShader)
    -- _ <- liftJSM $ jsg (T.pack "console") ^. js1 (T.pack "log") (gl ^. js1 (T.pack "getShaderInfoLog") fragmentShader)

    program <- createProgram gl
    attachShader gl (Just program) (Just vertexShader)
    attachShader gl (Just program) (Just fragmentShader)
    linkProgram gl (Just program)
    -- _ <- liftJSM $ jsg (T.pack "console") ^. js1 (T.pack "log") (gl ^. js1 (T.pack "getProgramInfoLog") program)

    positionLocation <- getAttribLocation gl (Just program) ("a_position" :: Text)
    windowSizeLocation <- getUniformLocation gl (Just program) ("u_windowSize" :: Text)
    posLocation <- getUniformLocation gl (Just program) ("u_pos" :: Text)
    sizeLocation <- getUniformLocation gl (Just program) ("u_size" :: Text)
    extraDataLocation <- getUniformLocation gl (Just program) ("u_extraData" :: Text)

    let compiledProgram = program
    return CompiledProgram {..}

type ExtraData = (Double, (Double, Double), Double)

paintGLCached :: (Ord a, MonadDOM m) =>
    Cache m a CompiledProgram ->
    WebGLRenderingContext -> (Double, Double) -> [(a, ExtraData)] -> m ()
paintGLCached pgmCache gl pos toDraw = do
    pgms <- compileCached pgmCache toDraw
    paintGL gl pos pgms

paintGL :: MonadDOM m =>
    WebGLRenderingContext -> (Double, Double) -> [(CompiledProgram, ExtraData)] -> m ()
paintGL gl (w,h) toDraw = do
    bw <- getDrawingBufferWidth gl
    bh <- getDrawingBufferHeight gl
    viewport gl 0 0 bw bh

    clearColor gl 1 1 1 0
    clear gl COLOR_BUFFER_BIT

    for_ toDraw $ \(CompiledProgram {..}, (extraData, (x,y), size)) -> do
        enableVertexAttribArray gl (fromIntegral positionLocation)
        vertexAttribPointer gl (fromIntegral positionLocation) 2 FLOAT False 0 0

        useProgram gl (Just compiledProgram)

        uniform2f gl (Just windowSizeLocation) w h
        uniform2f gl (Just posLocation) x y
        uniform1f gl (Just sizeLocation) size
        uniform1f gl (Just extraDataLocation) extraData

        drawArrays gl TRIANGLES 0 6

querySize :: (IsElement self, MonadJSM m) => self -> m (Double, Double)
querySize domEl = liftJSM $ do
      w <- getClientWidth domEl
      h <- getClientHeight domEl
      return (w,h)

autoResizeCanvas ::
    (MonadFix m, MonadHold t m,
    MonadJSM (Performable m), MonadJSM m, PerformEvent t m,
    TriggerEvent t m) =>
    Event t () ->
    HTMLCanvasElement -> m (Dynamic t (Double, Double), Event t ())
autoResizeCanvas eMayHaveChanged domEl =  do
  -- Automatically update the size when it changes
  ratio <- liftJSM $ currentWindow >>= getDevicePixelRatio . fromJust
  initialSize <- querySize domEl
  eCanvasSize <- performEvent (querySize domEl <$ eMayHaveChanged)
  dCanvasSize <- holdUniqDyn =<< holdDyn initialSize eCanvasSize
  eResized <- performEvent $ (<$> updated dCanvasSize) $ \(w,h) -> do
    setWidth domEl (ceiling (ratio * w))
    setHeight domEl (ceiling (ratio * h))
    return ()
  return (dCanvasSize, eResized)

type ResultStuff t =
    ( Event t (Double,Double)
    , Dynamic t (Double, Double)
    )

shaderCanvas ::
    Ord a =>
    MonadWidget t m =>
    (a -> Text) ->
    Event t () ->
    Dynamic t [(a, ExtraData)] ->
    m (ResultStuff t)
shaderCanvas toGLSL eMayHaveChanged toDraw
    = snd <$> shaderCanvas' toGLSL eMayHaveChanged toDraw

shaderCanvas' ::
    Ord a =>
    MonadWidget t m =>
    (a -> Text) ->
    Event t () ->
    Dynamic t [(a,ExtraData)] ->
    m (El t, ResultStuff t)
shaderCanvas' toGLSL eMayHaveChanged toDraw = do
    (canvasEl, _) <- el' "canvas" blank
    pb <- getPostBuild

    domEl <- unsafeCastTo HTMLCanvasElement $ _element_raw canvasEl
    (dCanvasSize, eResized) <- autoResizeCanvas eMayHaveChanged domEl

    eClick <- wrapDomEvent domEl (onEventName Mousedown) $
      bimap fromIntegral fromIntegral <$> mouseOffsetXY

    let eDraw = leftmost
          [ tag (current toDraw) eResized
          , updated toDraw
          , tag (current toDraw) pb
          ]

    getContext domEl ("experimental-webgl"::Text) ([]::[()]) >>= \case
      Nothing ->
        -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" vertexShader)
        return ()
      Just gl' -> do
        gl <- unsafeCastTo WebGLRenderingContext gl'
        cs <- commonSetup gl

        pgmCache <- newCache (compileFragmentShader gl cs . toGLSL)

        performEvent_ $
          paintGLCached pgmCache gl <$> current dCanvasSize <@> eDraw

    return (canvasEl, (eClick, dCanvasSize))

saveToPNG :: MonadJSM m => (a -> Text) -> [(a, ExtraData)] -> Text -> m ()
saveToPNG toGLSL toDraw name = do
    doc <- currentDocumentUnchecked
    domEl <- uncheckedCastTo HTMLCanvasElement <$> createElement doc ("canvas" :: Text)
    setWidth domEl 1000
    setHeight domEl 1000
    getContext domEl ("experimental-webgl"::Text) ([]::[()]) >>= \case
      Nothing -> return ()
      Just gl' -> do
        gl <- unsafeCastTo WebGLRenderingContext gl'
        cs <- commonSetup gl
        toDraw' <- forM toDraw $ \(a,x) -> do
            prog <- compileFragmentShader gl cs (toGLSL a)
            pure (prog,x)
        paintGL gl (1000, 1000) toDraw'
    CanvasSave.save name domEl

-- A compiled program cache

type Compiler m a b = a -> m b
type Cache m a b = IORef (Compiler m a b, M.Map a b)

newCache :: MonadIO m => (a -> m2 b) -> m (Cache m2 a b)
newCache f = liftIO $ newIORef (f, M.empty)

compileCached :: (Ord a, MonadIO m) => Cache m a b -> [(a,c)] -> m [(b,c)]
compileCached c xs = do
    (f, oldC) <- liftIO $ readIORef c
    let newMap = M.fromList $ map (()<$) xs
    newC <- M.mergeA
        M.dropMissing
        (M.traverseMissing (\k () -> f k))
        (M.zipWithMatched (\_ p _ -> p))
        oldC newMap
    liftIO $ writeIORef c (f, newC)
    return [(newC M.! k,d) | (k,d) <- xs]
