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
module Kaleidogen where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.Bifunctor
import Data.Functor

import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.Document
import GHCJS.DOM.Performance
import GHCJS.DOM.GlobalPerformance
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers (click)

import ShaderCanvas
import qualified CanvasSave
import qualified Animate
import Program

#if defined(ghcjs_HOST_OS)
run :: a -> a
run = id
#elif defined(MIN_VERSION_jsaddle_wkwebview)
import Language.Javascript.JSaddle.WKWebView (run)
#else
import qualified Language.Javascript.JSaddle.Warp (run)
run :: JSM () -> IO ()
run = Language.Javascript.JSaddle.Warp.run 3003
#endif


runInBrowser :: BackendRunner JSM
runInBrowser toShader go = run $ do
    doc <- currentDocumentUnchecked
    docEl <- getDocumentElementUnchecked doc
    setInnerHTML docEl html
    CanvasSave.register

    Just win <- currentWindow
    perf <- getPerformance win

    -- Get Dom elements
    canvas <- getElementByIdUnsafe doc ("canvas" :: Text) >>= unsafeCastTo HTMLCanvasElement
    save <- getElementByIdUnsafe doc ("save" :: Text) >>= unsafeCastTo HTMLAnchorElement
    del <- getElementByIdUnsafe doc ("delete" :: Text) >>= unsafeCastTo HTMLAnchorElement

    drawShaderCircles <- shaderCanvas toShader canvas

    let animate = Animate.animate

    let setCanDelete True = setClassName del (""::Text)
        setCanDelete False = setClassName del ("hidden"::Text)
    let setCanSave True = setClassName save (""::Text)
        setCanSave False = setClassName save ("hidden"::Text)
    let currentWindowSize = querySize canvas
    let getCurrentTime = now perf

    let onClick f = void $ on canvas click $ do
        pos <- bimap fromIntegral fromIntegral <$> mouseOffsetXY
        liftJSM (f pos)
    let onDel f = void $ on del click (liftJSM f)
    let onSave f = void $ on save click (liftJSM f)
    let onResize f = do
        checkResize <- autoResizeCanvas canvas f
        -- Wish I could use onResize on body, but that does not work somehow
        let regularlyCheckSize = do
            checkResize
            () <$ inAnimationFrame' (const regularlyCheckSize)
        regularlyCheckSize -- should trigger the initial render as well
        return ()
    let doSave filename toDraw = saveToPNG toShader toDraw filename

    go (Backend {..})


main :: IO ()
main = runInBrowser renderDNA mainProgram


html :: T.Text
html = T.unlines
    [ "<html>"
    , " <head>"
    , "  <style>" <> css <> "</style>"
    , "  <title>Kaleidogen</title>"
    , " </head>"
    , " <body>"
    , "  <div align='center'>"
    , "   <div class='toolbar'>"
    , "    <a id='delete'>🗑</a>"
    , "    <a id='save'>💾</a>"
    , "   </div>"
    , "   <canvas id='canvas'></canvas>"
    , "  </div>"
    , " </body>"
    , "</html>"
    ]

css :: T.Text
css = T.unlines
    [ "html {"
    , "  margin: 0;"
    , "  padding: 0;"
    , "  height: 100%;"
    , "}"
    , "body {"
    , "  display: flex;"
    , "  margin: 0;"
    , "  padding: 0;"
    , "  height: 100%;"
    , "  flex-direction: column;"
    , "}"
    , ".toolbar {"
    , "  height:10vh;"
    , "  width:95vw;"
    , "  margin:0;"
    , "  padding:0;"
    , "}"
    , ".toolbar a.hidden {"
    , "  display:none"
    , "}"
    , ".toolbar a {"
    , "  display:inline-block;"
    , "  margin:1vh 2vh;"
    , "  border:none;"
    , "  padding:.5vh;"
    , "  font-size:6vh;"
    , "  width:8vh;"
    , "  height:8vh;"
    , "  background-color:lightblue;"
    , "  border-radius: 1vh;"
    , "}"
    , ".toolbar a.disabled {"
    , "  background-color:lightgrey;"
    , "  color:white;"
    , "}"
    , "canvas {"
    , "  height:89vh;"
    , "  width:100%;"
    , "  margin:0;"
    , "  padding:0;"
    , "}"
    ]

