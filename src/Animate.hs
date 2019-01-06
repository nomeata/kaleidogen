{-# LANGUAGE ScopedTypeVariables #-}
module Animate (animate) where

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
