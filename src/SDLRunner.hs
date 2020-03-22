{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NondecreasingIndentation #-}
module SDLRunner where

import SDL hiding (clear)
import Linear (V4(..))
import Control.Monad (unless, forM_)
import Graphics.Rendering.OpenGL as GL
import Data.StateVar
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding
import Data.Functor
import Data.Function
import System.Exit

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Shaders
import Program

runInSDL :: BackendRunner IO
runInSDL toShader go = do
    initializeAll
    window <- createWindow "My SDL Application" $
        defaultWindow
            { windowGraphicsContext = OpenGLContext defaultOpenGL --  { glProfile = ES Normal 3 0 }
            , windowHighDPI = True
            , windowResizable = True
            }
    glContext <- glCreateContext window

    clearColor $= Color4 1 1 1 1

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

    arrayBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just arrayBuffer
    withArray vertices $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vertices))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    blend $= Enabled
    blendFunc $= (One, OneMinusSrcAlpha)

    let drawShaderCircles toDraw = do
        V2 bw bh <- glGetDrawableSize window
        viewport $= (Position 0 0, Size (fromIntegral bw) (fromIntegral bh))

        clear [GL.ColorBuffer]

        forM_ toDraw $ \(x,(a,b,c,d)) -> do
            let (vertexShaderSource, fragmentShaderSource) = toShader x
            program <- createProgram
            vertexShader <- createShader VertexShader
            shaderSourceBS vertexShader $= encodeUtf8 vertexShaderSource
            compileAndCheck vertexShader
            attachShader program vertexShader
            fragmentShader <- createShader FragmentShader
            shaderSourceBS fragmentShader $= encodeUtf8 fragmentShaderSource
            compileAndCheck fragmentShader
            attachShader program fragmentShader
            linkAndCheck program

            vPosition <- get (attribLocation program  "a_position")
            vertexAttribPointer vPosition $=
              (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
            vertexAttribArray vPosition $= Enabled

            currentProgram $= Just program

            V2 w h <- get (windowSize window)
            vWindowSize <- get (uniformLocation program  "u_windowSize")
            uniform vWindowSize $= Vector2 (fromIntegral w) (fromIntegral h::Float)

            vExtraData <- get (uniformLocation program  "u_extraData")
            uniform vExtraData $= Vector4 (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d::Float)

            bindVertexArrayObject $= Just triangles
            drawArrays Triangles 0 6

        glSwapWindow window

    let animate f = return (void $ f 100)

    let doSave filename toDraw = return ()

    let currentWindowSize = do
        V2 w h <- get (windowSize window)
        return (fromIntegral w,fromIntegral h)

    let getCurrentTime = (1000*) <$> time

    let setCanDelete _ = return ()
    let setCanSave _ = return ()

    Callbacks {..} <- go (Backend {..})

    currentWindowSize >>= onResize

    let render = do
        (toDraw, continue) <- onDraw
        drawShaderCircles toDraw
        return ()

    let handleEvent e = case eventPayload e of
            WindowResizedEvent _
                -> currentWindowSize >>= onResize

            KeyboardEvent keyboardEvent
                | keyboardEventKeyMotion keyboardEvent == Pressed
                , keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                -> exitSuccess

            MouseButtonEvent me
                | mouseButtonEventButton me == ButtonLeft
                , mouseButtonEventMotion me == Pressed
                , let P (V2 x y) = mouseButtonEventPos me
                -> onMouseDown (fromIntegral x, fromIntegral y)

            MouseButtonEvent me
                | mouseButtonEventButton me == ButtonLeft
                , mouseButtonEventMotion me == Released
                -> onMouseUp

            MouseMotionEvent me
                | let P (V2 x y) = mouseMotionEventPos me
                -> onMove (fromIntegral x, fromIntegral y)

            _ -> return ()

    fix $ \loop -> do
        es <- pollEvents
        unless (null es) $ mapM_ handleEvent es >> render
        waitEventTimeout (1000`div`60) >>= mapM_ handleEvent
        render
        loop

main :: IO ()
main = runInSDL renderGraphic mainProgram


bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


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
