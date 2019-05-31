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
    , autoResizeCanvas
    , querySize
    , shaderCanvas
    , saveToPNG
    ) where

import Data.Maybe
import Data.Text as Text (Text)
import Data.Foldable
import Data.Int
import Data.Monoid
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

import Language.Javascript.JSaddle.Object hiding (array)
-- import Control.Lens ((^.))

import CanvasSave
import Shaders


commonSetup :: MonadJSM m => WebGLRenderingContext -> m ()
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


data CompiledProgram = CompiledProgram
    { compiledProgram :: WebGLProgram
    , positionLocation :: Int32
    , windowSizeLocation  :: WebGLUniformLocation
    , extraDataLocation  :: WebGLUniformLocation
    }

compileFragmentShader :: MonadJSM m => WebGLRenderingContext -> Shaders -> m CompiledProgram
compileFragmentShader gl (vertexShaderSource, fragmentShaderSource) = do
    vertexShader <- createShader gl VERTEX_SHADER
    shaderSource gl (Just vertexShader) $
        "precision mediump float;\n" <>
        vertexShaderSource
    compileShader gl (Just vertexShader)
    -- _ <- liftJSM $ jsg (Text.pack "console") ^. js1 (Text.pack "log") (gl ^. js1 (Text.pack "getShaderInfoLog") vertexShader)

    fragmentShader <- createShader gl FRAGMENT_SHADER
    shaderSource gl (Just fragmentShader) $
        "precision mediump float;\n" <>
        fragmentShaderSource
    compileShader gl (Just fragmentShader)
    -- _ <- liftJSM $ jsg (Text.pack "console") ^. js1 (Text.pack "log") (gl ^. js1 (Text.pack "getShaderInfoLog") fragmentShader)

    program <- createProgram gl
    attachShader gl (Just program) (Just vertexShader)
    attachShader gl (Just program) (Just fragmentShader)
    linkProgram gl (Just program)
    -- _ <- liftJSM $ jsg (Text.pack "console") ^. js1 (Text.pack "log") (gl ^. js1 (Text.pack "getProgramInfoLog") program)

    positionLocation <- getAttribLocation gl (Just program) ("a_position" :: Text)
    windowSizeLocation <- getUniformLocation gl (Just program) ("u_windowSize" :: Text)
    extraDataLocation <- getUniformLocation gl (Just program) ("u_extraData" :: Text)

    let compiledProgram = program
    return CompiledProgram {..}

-- A single vec4 of extra data
type ExtraData = (Double, Double, Double, Double)

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

    for_ toDraw $ \(CompiledProgram {..}, (a,b,c,d)) -> do
        enableVertexAttribArray gl (fromIntegral positionLocation)
        vertexAttribPointer gl (fromIntegral positionLocation) 2 FLOAT False 0 0

        useProgram gl (Just compiledProgram)

        uniform2f gl (Just windowSizeLocation) w h
        uniform4f gl (Just extraDataLocation) a b c d

        drawArrays gl TRIANGLES 0 6

querySize :: (IsElement self, MonadJSM m) => self -> m (Double, Double)
querySize domEl = liftJSM $ do
      w <- getClientWidth domEl
      h <- getClientHeight domEl
      return (w,h)

autoResizeCanvas :: MonadJSM m => HTMLCanvasElement -> ((Double, Double) -> m ()) -> m (m ())
autoResizeCanvas domEl onResize =  do
  ratio <- liftJSM $ currentWindow >>= getDevicePixelRatio . fromJust
  sizeRef <- liftIO $ newIORef (0,0)
  return $ do
    lastSize <- liftIO $ readIORef sizeRef
    currentSize@(w,h) <- querySize domEl
    when (lastSize /= currentSize) $ do
        setWidth domEl (ceiling (ratio * w))
        setHeight domEl (ceiling (ratio * h))
        liftIO $ writeIORef sizeRef currentSize
        onResize currentSize

shaderCanvas ::
    Ord a =>
    MonadJSM m =>
    (a -> Shaders) ->
    HTMLCanvasElement ->
    m ([(a,ExtraData)] -> m ())
shaderCanvas toGLSL domEl =
    getContext domEl ("experimental-webgl"::Text) ([]::[()]) >>= \case
      Nothing ->
        -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" vertexShader)
        return (\_ -> return ())
      Just gl' -> do
        gl <- unsafeCastTo WebGLRenderingContext gl'
        commonSetup gl
        pgmCache <- newCache (compileFragmentShader gl . toGLSL)
        -- performEvent_ $ paintGLCached pgmCache gl <$> current dCanvasSize <@> eDraw
        return $ \toDraw -> do
            size <- querySize domEl
            paintGLCached pgmCache gl size toDraw

saveToPNG :: MonadJSM m => (a -> Shaders) -> (a, ExtraData) -> Text -> m ()
saveToPNG toGLSL (a,x) name = do
    doc <- currentDocumentUnchecked
    domEl <- uncheckedCastTo HTMLCanvasElement <$> createElement doc ("canvas" :: Text)
    setWidth domEl 1000
    setHeight domEl 1000
    getContext domEl ("experimental-webgl"::Text) ([]::[()]) >>= \case
      Nothing -> return ()
      Just gl' -> do
        gl <- unsafeCastTo WebGLRenderingContext gl'
        prog <- compileFragmentShader gl (toGLSL a)
        paintGL gl (1000, 1000) [(prog,x)]
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
