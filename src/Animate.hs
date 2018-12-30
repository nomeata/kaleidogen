{-# LANGUAGE ScopedTypeVariables #-}
module Animate (animate, tween) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef

import GHCJS.DOM
import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM.Performance
import GHCJS.DOM.GlobalPerformance

animate :: (Double -> JSM Bool) -> JSM (JSM ())
animate draw = do
    Just win <- currentWindow
    perf <- getPerformance win
    animating <- liftIO $ newIORef False

    let continueAnimation t = do
        continue <- draw t
        if continue then () <$ inAnimationFrame' continueAnimation
                    else liftIO $ writeIORef animating False


    let drawAndAnimate t = do
        continue <- draw t
        -- If there is something to animate
        when continue $ do
           -- And no animation loop is running
            isAnimating <- liftIO (readIORef animating)
            unless isAnimating $ do
                liftIO $ writeIORef animating True
                () <$ inAnimationFrame' continueAnimation

    return $ do
        t <- now perf
        drawAndAnimate t

class Tweenable a where
    tween :: Double -> a -> a -> a

instance Tweenable Double where
    tween r a b
        | r < 0 = a
        | r > 1 = b
        | otherwise = (1-r) * a + r * b
instance (Tweenable a, Tweenable b) => Tweenable (a,b) where
    tween r (x,y) (x',y') = (tween r x x', tween r y y')
