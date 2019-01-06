{-# LANGUAGE ScopedTypeVariables #-}
module Tween (tween) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef

class Tweenable a where
    tween :: Double -> a -> a -> a

instance Tweenable Double where
    tween r a b
        | r < 0 = a
        | r > 1 = b
        | otherwise = (1-r) * a + r * b
instance (Tweenable a, Tweenable b) => Tweenable (a,b) where
    tween r (x,y) (x',y') = (tween r x x', tween r y y')
