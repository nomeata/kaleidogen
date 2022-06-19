{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Shapes
  ( dnaGraphic
  , borderGraphic
  , mouseGraphic
  , fullDNAGraphic
  )
where

import Shaders
import Expression
import GLSL
import DNA
import Layout

dnaShaders :: DNA -> Shaders
dnaShaders d = (circularVertexShader, toFragmentShader (dna2rna d))

dnaGraphic :: DNA -> Bool -> PosAndScale -> Double -> Graphic
dnaGraphic d active ((x,y),s) f = (dnaShaders d, (extraData, x, y, s, f))
  where
    extraData | active    = 0
              | otherwise = 3

borderGraphic :: Double -> Graphic
borderGraphic radius = (borderShaders, (0, 0, 0, radius, 1))

mouseShaders :: Shaders
mouseShaders = (circularVertexShader, mouseFragmentShader)

mouseGraphic :: Bool -> PosAndScale -> Graphic
mouseGraphic pressed ((x,y),s) = (mouseShaders, (extraData, x, y, s, 1))
  where
    extraData | pressed   = 1
              | otherwise = 0

fullDNAGraphic :: DNA -> (Double,Double) -> Graphic
fullDNAGraphic dna (w,h) = (dnaShaders dna, (0, x, y, s, 1))
  where
    ((x,y),s) = layoutFullCirlce (w,h) ()

