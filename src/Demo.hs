{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
module Demo (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor
import qualified Text.Hex as Hex
import qualified Data.ByteString as BS
import Data.IORef
import Control.Monad.Trans

import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.Document
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers (input)
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea

import ShaderCanvas
import qualified CanvasSave
import Program
import RunWidget
import GLSL
import Expression
import PrettyRna

main :: IO ()
main = runWidget mainWidget

mainWidget :: JSM ()
mainWidget = do
    doc <- currentDocumentUnchecked
    docEl <- getDocumentElementUnchecked doc
    setInnerHTML docEl html
    CanvasSave.register

    -- Get Dom elements
    canvas1 <- getElementByIdUnsafe doc ("canvas1" :: Text) >>= unsafeCastTo HTMLCanvasElement
    input1 <- getElementByIdUnsafe doc ("input1" :: Text) >>= unsafeCastTo HTMLInputElement
    tree1 <- getElementByIdUnsafe doc ("tree1" :: Text) >>= unsafeCastTo HTMLTextAreaElement
    code1 <- getElementByIdUnsafe doc ("code1" :: Text) >>= unsafeCastTo HTMLTextAreaElement

    dnaRef1 <- liftIO $ newIORef []
    sizeRef <- liftIO $ newIORef (100,100)


    drawShaderCircles <- shaderCanvas renderGraphic canvas1

    let draw = do
        dna1 <- liftIO $ readIORef dnaRef1
        canvassize <- liftIO $ readIORef sizeRef
        let pretty = prettyRNA (dna2rna dna1)
        let rows = fromIntegral (length (lines pretty))
        TextArea.setValue tree1 pretty
        TextArea.setValue code1 (toFragmentShader (dna2rna dna1))
        TextArea.setRows tree1 rows
        TextArea.setRows code1 rows
        drawShaderCircles [showFullDNA dna1 canvassize]

    void $ on input1 input $ do
        str <- Input.getValue input1
        case Hex.decodeHex str of
          Just x | not (BS.null x) ->
            liftIO (writeIORef dnaRef1 (BS.unpack x)) >> lift draw
          _ -> return ()

    checkResize <- autoResizeCanvas canvas1 (\pos -> liftIO (writeIORef sizeRef pos) >> draw)
    -- Wish I could use onResize on body, but that does not work somehow
    let regularlyCheckSize = do
        checkResize
        () <$ inAnimationFrame' (const regularlyCheckSize)
    regularlyCheckSize -- should trigger the initial render as well



html :: T.Text
html = T.unlines
    [ "<html>"
    , " <head>"
    , "  <style>" <> css <> "</style>"
    , "  <title>Kaleidogen</title>"
    , " </head>"
    , " <body>"
    , "  <div align='center'>"
    , "   <input type='text' value='FFFF' id='input1'>"
    , "  </div>"
    , "  <div align='center'>"
    , "   <div id='textareas'>"
    , "    <textarea id='tree1' rows='1'></textarea>"
    , "    <textarea id='code1' rows='1'></textarea>"
    , "   </div>"
    , "  </div>"
    , "  <div align='center'>"
    , "   <canvas width='1000' height='1000' id='canvas1'></canvas>"
    , "  </div>"
    , " </body>"
    , "</html>"
    ]

css :: T.Text
css = T.unlines
    [ "html {"
    , "}"
    , "canvas {"
    , "  height:60vh;"
    , "  width:60vh;"
    , "}"
    , "#textareas {"
    , "  width:80%;"
    , "  display:flex;"
    , "}"
    , "textarea {"
    , "  width:100%;"
    , "  font-family: monospace;"
    , "  resize: none;"
    , "}"
    , "#code {"
    , "  resize: vertical;"
    , "}"
    , "input {"
    , "  width:80%;"
    , "  font-size:200%;"
    , "}"
    ]

