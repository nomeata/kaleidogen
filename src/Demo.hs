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
import GHCJS.DOM.GlobalEventHandlers (input, change)
import qualified GHCJS.DOM.HTMLElement as Element
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea

import ShaderCanvas
import qualified CanvasSave
import Program
import RunWidget
import GLSL
import DNA
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

    col2 <- getElementByIdUnsafe doc ("col2" :: Text) >>= unsafeCastTo HTMLDivElement
    canvas2 <- getElementByIdUnsafe doc ("canvas2" :: Text) >>= unsafeCastTo HTMLCanvasElement
    input2 <- getElementByIdUnsafe doc ("input2" :: Text) >>= unsafeCastTo HTMLInputElement
    tree2 <- getElementByIdUnsafe doc ("tree2" :: Text) >>= unsafeCastTo HTMLTextAreaElement
    code2 <- getElementByIdUnsafe doc ("code2" :: Text) >>= unsafeCastTo HTMLTextAreaElement

    col3 <- getElementByIdUnsafe doc ("col3" :: Text) >>= unsafeCastTo HTMLDivElement
    canvas3 <- getElementByIdUnsafe doc ("canvas3" :: Text) >>= unsafeCastTo HTMLCanvasElement
    input3 <- getElementByIdUnsafe doc ("input3" :: Text) >>= unsafeCastTo HTMLInputElement
    tree3 <- getElementByIdUnsafe doc ("tree3" :: Text) >>= unsafeCastTo HTMLTextAreaElement
    code3 <- getElementByIdUnsafe doc ("code3" :: Text) >>= unsafeCastTo HTMLTextAreaElement

    dnaRef1 <- liftIO $ newIORef [0xFF]
    dnaRef3 <- liftIO $ newIORef [0x01]
    sizeRef <- liftIO $ newIORef (100,100)


    drawShaderCircles1 <- shaderCanvas renderGraphic canvas1
    drawShaderCircles2 <- shaderCanvas renderGraphic canvas2
    drawShaderCircles3 <- shaderCanvas renderGraphic canvas3

    let drawOne dna tree code drawShaderCircles = do
        canvassize <- liftIO $ readIORef sizeRef
        let pretty = prettyRNA (dna2rna dna)
        let rows = fromIntegral (length (lines pretty))
        TextArea.setValue tree pretty
        TextArea.setValue code (toFragmentShader (dna2rna dna))
        TextArea.setRows tree rows
        TextArea.setRows code rows
        drawShaderCircles [showFullDNA dna canvassize]

    let draw = do
        dna1 <- liftIO $ readIORef dnaRef1
        dna3 <- liftIO $ readIORef dnaRef3
        let dna2 = crossover 0 dna1 dna3
        Input.setValue input2 (T.toUpper (Hex.encodeHex (BS.pack dna2)))
        drawOne dna1 tree1 code1 drawShaderCircles1
        drawOne dna2 tree2 code2 drawShaderCircles2
        drawOne dna3 tree3 code3 drawShaderCircles3

    let enterDna inputElem ref = void $ on inputElem input $ do
            str <- Input.getValue inputElem
            case Hex.decodeHex str of
              Just x | not (BS.null x) -> do
                liftIO $ writeIORef ref (BS.unpack x)
                lift draw
              _ -> return ()
    enterDna input1 dnaRef1
    enterDna input3 dnaRef3

    let whenChecked element_id f = do
        el <- getElementByIdUnsafe doc (element_id :: Text) >>= unsafeCastTo HTMLInputElement
        let check = do
            checked <- Input.getChecked el
            f checked
            draw
        void $ on el change (lift check)
        check


    whenChecked "showcross" $ \checked -> do
      Element.setHidden col2 (not checked)
      Element.setHidden col3 (not checked)
    whenChecked "showcode" $ \checked -> do
      Element.setHidden code1 (not checked)
      Element.setHidden code2 (not checked)
      Element.setHidden code3 (not checked)
    whenChecked "showrna" $ \checked -> do
      Element.setHidden tree1 (not checked)
      Element.setHidden tree2 (not checked)
      Element.setHidden tree3 (not checked)

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
    , "   <label><input type='checkbox' id='showrna'>RNA</input></label>"
    , "   <label><input type='checkbox' id='showcode'>Code</input></label>"
    , "   <label><input type='checkbox' id='showcross'>Breeding</input></label>"
    , "  </div>"
    , "  <div class='threecols'>"
    , "   <div id='col1' class='col'>"
    , "    <div align='center'>"
    , "     <input class='dna' type='text' value='FF' id='input1' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false'>"
    , "    </div>"
    , "    <div align='center'>"
    , "     <canvas width='1000' height='1000' id='canvas1'></canvas>"
    , "    </div>"
    , "    <div align='center'>"
    , "     <div class='textareas'>"
    , "      <textarea id='tree1' rows='1'></textarea>"
    , "      <textarea id='code1' rows='1'></textarea>"
    , "     </div>"
    , "    </div>"
    , "   </div>"
    , "   <div id='col2' class='col'>"
    , "    <div align='center'>"
    , "     <input class='dna' type='text' value='FF' id='input2' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false'>"
    , "    </div>"
    , "    <div align='center'>"
    , "     <canvas width='1000' height='1000' id='canvas2'></canvas>"
    , "    </div>"
    , "    <div align='center'>"
    , "     <div class='textareas'>"
    , "      <textarea id='tree2' rows='2'></textarea>"
    , "      <textarea id='code2' rows='2'></textarea>"
    , "     </div>"
    , "    </div>"
    , "   </div>"
    , "   <div id='col3' class='col'>"
    , "    <div align='center'>"
    , "     <input class='dna' type='text' value='01' id='input3' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false'>"
    , "    </div>"
    , "    <div align='center'>"
    , "     <canvas width='1000' height='1000' id='canvas3'></canvas>"
    , "    </div>"
    , "    <div align='center'>"
    , "     <div class='textareas'>"
    , "      <textarea id='tree3' rows='1'></textarea>"
    , "      <textarea id='code3' rows='1'></textarea>"
    , "     </div>"
    , "    </div>"
    , "   </div>"
    , "  </div id='threecols'>"
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
    , ".threecols {"
    , "  width:100%;"
    , "  display:flex;"
    , "}"
    , ".col {"
    , "  width:100%;"
    , "}"
    , ".textareas {"
    , "  width:80%;"
    , "  display:flex;"
    , "}"
    , "textarea {"
    , "  width:100%;"
    , "  font-family: monospace;"
    , "  resize: none;"
    , "}"
    , "#code1, #code2, #code3 {"
    , "  resize: vertical;"
    , "}"
    , "input.dna {"
    , "  width:80%;"
    , "  font-size:200%;"
    , "}"
    ]

