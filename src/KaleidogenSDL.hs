{-# LANGUAGE OverloadedStrings #-}
module KaleidogenSDL where

import SDL hiding (clear)
import Linear (V4(..))
import Control.Monad (unless)
import Graphics.Rendering.OpenGL as GL
import Data.StateVar
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import GLSL
import Expression
import Shaders

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" $
    defaultWindow
      { windowOpenGL = Just defaultOpenGL --  { glProfile = ES Normal 3 0 }
      , windowHighDPI = True
      }
  glContext <- glCreateContext window
  appLoop window

interestingShader :: BS.ByteString
interestingShader =
  encodeUtf8 $
  toFragmentShader $
  dna2rna
  [50,200,3,124,5,6,7]

trivialFragmentShader :: BS.ByteString
trivialFragmentShader = BS.unlines
  [ "varying vec2 vDrawCoord;"
  , "void main() {"
  , "  vec2 pos = vDrawCoord;"
  , "  // pos is a scaled pixel position, (0,0) is in the center of the canvas"
  , "  // If the position is outside the inscribed circle, make it transparent"
  , "  if (length(pos) > 1.0) { gl_FragColor = vec4(0,0,0,0); return; }"
  , "  // Otherwise, return red"
  , "  gl_FragColor = vec4(1.0,0.0,0.0,1.0);"
  , "}"
  ]

appLoop :: Window -> IO ()
appLoop window = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  clearColor $= Color4 1 1 1 1
  clear [GL.ColorBuffer]

  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles

  let vertices = [
        Vertex2 (-1) (-1),  -- Triangle 1
        Vertex2   1  (-1),
        Vertex2 (-1)   1 ,
        Vertex2   1  (-1),  -- Triangle 2
        Vertex2   1    1 ,
        Vertex2 (-1)   1 ] :: [Vertex2 GLfloat]
      numVertices = length vertices

  blend $= Enabled
  blendFunc $= (One, OneMinusSrcAlpha)

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  program <- createProgram
  shader <- createShader VertexShader
  shaderSourceBS shader $= encodeUtf8 vertexShaderSource
  compileAndCheck shader
  attachShader program shader
  shader <- createShader FragmentShader
  shaderSourceBS shader $= interestingShader
  compileAndCheck shader
  attachShader program shader
  linkAndCheck program

  vPosition <- get (attribLocation program  "a_position")
  vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
  vertexAttribArray vPosition $= Enabled

  V2 bw bh <- glGetDrawableSize window
  viewport $= (Position 0 0, Size (fromIntegral bw) (fromIntegral bh))

  currentProgram $= Just program

  V2 w h <- get (windowSize window)
  vWindowSize <- get (uniformLocation program  "u_windowSize")
  uniform vWindowSize $= Vector2 (fromIntegral w) (fromIntegral h::Float)

  vExtraData <- get (uniformLocation program  "u_extraData")
  uniform vExtraData $= Vector4 (0::Float) 100 100 50

  bindVertexArrayObject $= Just triangles
  drawArrays Triangles 0 6

  glSwapWindow window

  unless qPressed (appLoop window)


-- copied from https://github.com/madjestic/Haskell-OpenGL-Tutorial/blob/master/tutorial02/LoadShaders.hs
linkAndCheck :: Program -> IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

compileAndCheck :: Shader -> IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

checked :: (t -> IO ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> IO ()
checked action getStatus getInfoLog message object = do
   action object
   ok <- get (getStatus object)
   unless ok $ do
      infoLog <- get (getInfoLog object)
      fail (message ++ " log: " ++ infoLog)
