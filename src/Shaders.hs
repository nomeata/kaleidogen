{-# LANGUAGE OverloadedStrings #-}
module Shaders where

import Data.Text as Text

type Shaders = (Text, Text)

circularVertexShader :: Text
circularVertexShader = Text.unlines
  [ "attribute vec2 a_position;"
  , "uniform vec2 u_windowSize;"
  , "uniform vec4 u_extraData;"
  , "varying vec2 vDrawCoord;"
  , "void main() {"
  , "  vec2 pos = u_extraData.yz;"
  , "  float size = u_extraData.w;"
  , "  vDrawCoord = vec2(a_position);"
  , "  vec2 scaled_pos = vec2(1.0,-1.0) * (2.0 * (size * a_position + pos)/u_windowSize - vec2(1,1));"
  , "  gl_Position = vec4(scaled_pos, 0, 1);"
  , "}"
  ]

-- | An example fragment shader program, drawing a red circle
circularTrivialFragmentShader :: Text
circularTrivialFragmentShader = Text.unlines
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

borderShaders :: Shaders
borderShaders = (borderVertexShader, borderFragmentShader)

borderVertexShader :: Text
borderVertexShader = Text.unlines
  [ "attribute vec2 a_position;"
  , "varying vec2 vDrawCoord;"
  , "void main() {"
  , "  vDrawCoord = vec2(a_position);"
  , "  vec2 scaled_pos = vec2(1.0,-1.0) * a_position;"
  , "  gl_Position = vec4(scaled_pos, 0, 1);"
  , "}"
  ]

borderFragmentShader :: Text
borderFragmentShader = Text.unlines
  [ "precision mediump float;"
  , "varying vec2 vDrawCoord;"
  , "uniform vec2 u_windowSize;"
  , "uniform vec4 u_extraData;"
  , "void main() {"
  , "  float r = u_extraData.w;"
  , "  vec2 pos = abs(((abs(vDrawCoord)) * u_windowSize) - u_windowSize);"
  , "  if (length(pos) > r) { gl_FragColor = vec4(0,0,0,0); return; }"
  , "  if (length(vec2(r) - pos) < r) { gl_FragColor = vec4(0,0,0,0); return; }"
  , "  gl_FragColor = vec4(0.0,0.0,0.0,1.0);"
  , "}"
  ]
