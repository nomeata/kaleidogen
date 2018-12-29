{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Animate (interpolate) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef

import GHCJS.DOM
import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM.Performance
import GHCJS.DOM.GlobalPerformance

import Layout (PosAndScale)

type Moving a = [(a, PosAndScale, Double, PosAndScale)]
type LaidOut a = [(a, PosAndScale)]

interpolate :: Double -> (LaidOut a -> JSM ()) -> JSM (Moving a -> JSM ())
interpolate speed draw = do
    Just win <- currentWindow
    perf <- getPerformance win
    s <- liftIO $ newIORef []
    animating <- liftIO $ newIORef False

    let drawInterp t = do
        moving <- liftIO $ readIORef s
        draw (interp t moving)
        return $ needsAnimation t moving

    let animate t = do
        continue <- drawInterp t
        if continue then () <$ inAnimationFrame' animate
                    else liftIO $ writeIORef animating False


    let drawAndAnimate t = do
        -- Draw current state
        continue <- drawInterp t
        -- If there is something to animate
        when continue $ do
           -- And no animation loop is running
            isAnimating <- liftIO (readIORef animating)
            unless isAnimating $ do
                liftIO $ writeIORef animating True
                () <$ inAnimationFrame' animate

    return $ \input -> do
        liftIO $ writeIORef s input
        t <- now perf
        drawAndAnimate t
  where
    interp :: Double -> Moving a -> LaidOut a
    interp t = map (interPos t)

    interPos t (a, ((x,y),s), t',((x',y'), s'))
        | let r = (t-t') / speed, r < 1,
          let f = tween r
        = (a,((f x' x, f y' y), f s' s))
        | otherwise
        = (a,((x,y),s))

    needsAnimation t = any (isChanging t)

    isChanging t (_, _, t',_) = (t-t') / speed < 1

class Tweenable a where
    tween :: Double -> a -> a -> a

instance Tweenable Double where
    tween r a b
        | r < 0 = a
        | r > 1 = b
        | otherwise = (1-r) * a + r * b
instance (Tweenable a, Tweenable b) => Tweenable (a,b) where
    tween r (x,y) (x',y') = (tween r x x', tween r y y')
