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
    canvas <- getElementByIdUnsafe doc ("canvas" :: Text) >>= unsafeCastTo HTMLCanvasElement
    input1 <- getElementByIdUnsafe doc ("input1" :: Text) >>= unsafeCastTo HTMLInputElement
    tree <- getElementByIdUnsafe doc ("tree" :: Text) >>= unsafeCastTo HTMLTextAreaElement
    code <- getElementByIdUnsafe doc ("code" :: Text) >>= unsafeCastTo HTMLTextAreaElement

    dna1 <- liftIO $ newIORef []
    size <- liftIO $ newIORef (100,100)


    drawShaderCircles <- shaderCanvas renderGraphic canvas

    let draw = do
        dna <- liftIO $ readIORef dna1
        canvassize <- liftIO $ readIORef size
        let pretty = prettyRNA (dna2rna dna)
        let rows = fromIntegral (length (lines pretty))
        TextArea.setValue tree pretty
        TextArea.setValue code (toFragmentShader (dna2rna dna))
        TextArea.setRows tree rows
        TextArea.setRows code rows
        drawShaderCircles [showFullDNA dna canvassize]

    void $ on input1 input $ do
        str <- Input.getValue input1
        case Hex.decodeHex str of
          Just x | not (BS.null x) ->
            liftIO (writeIORef dna1 (BS.unpack x)) >> lift draw
          _ -> return ()

    checkResize <- autoResizeCanvas canvas (\pos -> liftIO (writeIORef size pos) >> draw)
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
    , "    <textarea id='tree' rows='1'></textarea>"
    , "    <textarea id='code' rows='1'></textarea>"
    , "   </div>"
    , "  </div>"
    , "  <div align='center'>"
    , "   <canvas width='1000' height='1000' id='canvas'></canvas>"
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

