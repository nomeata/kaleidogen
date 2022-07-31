{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module ProgramScript
  ( scriptProgram )
where

import Control.Monad.Ref

import qualified Presentation
import qualified Tutorial as Tut
import Tween
import Program
import Shapes

scriptProgram :: MonadRef m => Program m -> Program m
scriptProgram mainP _st _seed t0 size0 = do
    let tutorialSeed = 4 -- fixed, chosen to look good.
    progRef <- mainP Nothing tutorialSeed t0 size0 >>= newRef
    let withProg act = readRef progRef >>= act
    let getPosOf t d = withProg $ \p -> resolveDest p t d

    -- Script playing state
    scriptRef <- newRef Tut.tutorial
    scriptStepStartRef <- newRef t0
    lastMousePosition <- getPosOf t0 Tut.Center >>= newRef
    currentMousePos <- readRef lastMousePosition >>= newRef
    mouseDownRef <- newRef False

    -- Need screen size to size the mouse
    sizeRef <- newRef size0

    let tickAnimation t = do
            s <- readRef scriptRef
            (x1,y1) <- readRef lastMousePosition
            t1 <- readRef scriptStepStartRef
            case s of
                [] -> pure ()
                (Tut.Wait n):s'
                  | t > t1 + n -> do
                    -- Wait is done
                    writeRef scriptStepStartRef (t1 + n)
                    writeRef scriptRef s'
                    tickAnimation t
                  | otherwise -> pure () -- Wait is not done
                (Tut.MoveMouseTo d n):s'
                  | t > t1 + n -> do
                    -- Mouse move is done
                    (x,y) <- getPosOf (t1 + n) d
                    withProg $ \p -> onMove p (t1 + n) (x,y)
                    writeRef scriptStepStartRef (t1 + n)
                    writeRef lastMousePosition (x,y)
                    writeRef currentMousePos (x,y)
                    writeRef scriptRef s'
                    tickAnimation t
                  | otherwise -> do
                    -- Mouse move is happening
                    (x,y) <- getPosOf (t1 + n) d
                    let mousePos = tween ((t-t1)/n) (x1,y1) (x,y)
                    withProg $ \p -> onMove p (t1 + n) mousePos
                    writeRef currentMousePos mousePos
                Tut.MouseDown:s' -> do
                    withProg $ \p -> onMouseDown p t1 (x1, y1)
                    writeRef scriptRef s'
                    writeRef mouseDownRef True
                    tickAnimation t
                Tut.MouseUp:s' -> do
                    withProg $ \p -> onMouseUp p t1
                    writeRef scriptRef s'
                    writeRef mouseDownRef False
                    tickAnimation t
                Tut.StartAnim:s' -> do
                    withProg $ \p -> onAnim p t1
                    writeRef scriptRef s'
                    tickAnimation t

    return $ Callbacks
        { onDraw = \t -> do
            tickAnimation t

            dr <- withProg $ \p -> onDraw p t

            -- Mouse pointer
            isMouseDown <- readRef mouseDownRef
            (mx,my) <- readRef currentMousePos
            (w,h) <- readRef sizeRef
            let ms = min (w/30) (h/30)
            let mouseObject = mouseGraphic isMouseDown ((mx,my), ms)

            stillAnimating <- Presentation.Animating . not . null <$> readRef scriptRef

            pure $ dr
                { objects = objects dr ++ [mouseObject]
                , stillAnimating = stillAnimating
                , tutInProgress = True
                }

        , onSerialize = withProg onSerialize -- should be dead code
        , onMouseDown = \_ _ -> pure ()
        , onMove      = \_ _ -> pure ()
        , onMouseUp   = \_ -> pure ()
        , onMouseOut  = \_ -> pure ()
        , onDel       = \_ -> pure ()
        , onAnim      = \_ -> pure ()
        , onSave      = \_ -> pure Nothing
        , onReset     = \_ _ -> pure ()
        , onTut       = \_ -> pure ()
        , onResize = \t size -> do
            writeRef sizeRef size
            -- This is a problem: resizing completely messes up with ongoing scripted interaction.
            -- Possible solution: The tutorial animation mouse movement is just for show,
            -- and it generates abstract Logic events instead.
            -- So just replay the whole animation with the new screen size! (Using same seed)
            mainP Nothing tutorialSeed t0 size >>= writeRef progRef
            writeRef scriptRef Tut.tutorial
            writeRef scriptStepStartRef t0

            getPosOf t0 Tut.Center >>= writeRef lastMousePosition
            readRef lastMousePosition >>= writeRef currentMousePos
            writeRef mouseDownRef False
            -- And now replay
            tickAnimation t
        , resolveDest = \t d -> withProg $ \p -> resolveDest p t d
        }
