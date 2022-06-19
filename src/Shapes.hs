{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Shapes
  ( Shape(..)
  , renderShape
  , fullDNAGraphic
  )
where

import Data.Text (Text)

import Shaders
import Expression
import GLSL
import DNA
import Layout

data Shape = DNA DNA | Border | Mouse deriving (Eq, Ord)

renderShape :: Shape -> (Text, Text)
renderShape (DNA d) = (circularVertexShader, toFragmentShader (dna2rna d))
renderShape Border = borderShaders
renderShape Mouse  = (circularVertexShader, mouseFragmentShader)

fullDNAGraphic :: DNA -> (Double,Double) -> Graphic
fullDNAGraphic dna (w,h) = (shaders, (0, x, y, s, 1))
  where
    shaders = renderShape (DNA dna)
    ((x,y),s) = layoutFullCirlce (w,h) ()

