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
{-# LANGUAGE LambdaCase #-}
module Kaleidogen (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Bifunctor
import Data.Functor
import Control.Monad.IO.Class
import Control.Monad.Random.Strict (getRandom)

import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.Document hiding (getLocation)
import GHCJS.DOM.Window (getLocation)
import GHCJS.DOM.Performance
import GHCJS.DOM.Location (getHash)
import GHCJS.DOM.GlobalPerformance
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers (click, mouseDown, mouseMove, mouseUp, mouseLeave, touchStart, touchMove, touchEnd)
import qualified GHCJS.DOM.TouchEvent as TouchEvent
import qualified GHCJS.DOM.TouchList as TouchList
import qualified GHCJS.DOM.Touch as Touch
import qualified GHCJS.DOM.DOMRect as DOMRect

import ShaderCanvas
import qualified CanvasSave
import qualified Animate
import Program
import RunWidget

main :: IO ()
main = runWidget mainWidget

mainWidget :: JSM ()
-- mainWidget = runInBrowser renderGraphic mainProgram
-- mainWidget = runInBrowser renderGraphic tutorialProgram
mainWidget = runInBrowser renderGraphic (switchProgram mainProgram tutorialProgram)

runInBrowser :: ProgramRunner JSM
runInBrowser toShader go = do
    doc <- currentDocumentUnchecked
    docEl <- getDocumentElementUnchecked doc
    setInnerHTML docEl html
    CanvasSave.register

    Just win <- currentWindow
    perf <- getPerformance win

    -- Get Dom elements
    canvas <- getElementByIdUnsafe doc ("canvas" :: Text) >>= unsafeCastTo HTMLCanvasElement
    save <- getElementByIdUnsafe doc ("save" :: Text) >>= unsafeCastTo HTMLAnchorElement
    anim <- getElementByIdUnsafe doc ("anim" :: Text) >>= unsafeCastTo HTMLAnchorElement
    del <- getElementByIdUnsafe doc ("delete" :: Text) >>= unsafeCastTo HTMLAnchorElement
    tut <- getElementByIdUnsafe doc ("tut" :: Text) >>= unsafeCastTo HTMLAnchorElement

    drawShaderCircles <- shaderCanvas toShader canvas

    loc <- getLocation win
    isTelegram <- ("tgShareScoreUrl" `T.isInfixOf`) <$> getHash loc

    let confButton e True  _     = setClassName e ("progress"::Text)
        confButton e False True  = setClassName e (""::Text)
        confButton e False False = setClassName e ("disabled"::Text)
    size0 <- querySize canvas
    t0 <- now perf
    seed0 <- liftIO getRandom

    Callbacks{..} <- go seed0 t0 size0

    render <- Animate.animate $ \_ -> do
        t <- now perf
        DrawResult {..} <- onDraw t
        drawShaderCircles objects
        confButton del  False canDelete
        confButton save False (not isTelegram && canSave)
        confButton anim animInProgress canAnim
        confButton tut  tutInProgress  True
        return stillAnimating

    void $ on canvas mouseDown $ do
        t <- now perf
        pos <- bimap fromIntegral fromIntegral <$> mouseOffsetXY
        liftJSM (onMouseDown t pos >> render)

    void $ on canvas mouseMove $ do
        t <- now perf
        pos <- bimap fromIntegral fromIntegral <$> mouseOffsetXY
        liftJSM (onMove t pos >> render)

    void $ on canvas mouseUp $ do
        t <- now perf
        liftJSM (onMouseUp t >> render)
    void $ on canvas mouseLeave $ do
        t <- now perf
        liftJSM (onMouseOut t >> render)

    void $ on canvas touchStart $ do
        t <- now perf
        pos <- touchOffsetXY canvas
        liftJSM (onMouseDown t pos >> render)
        preventDefault

    void $ on canvas touchMove $ do
        t <- now perf
        pos <- touchOffsetXY canvas
        liftJSM (onMove t pos >> render)

    void $ on canvas touchEnd $ do
        t <- now perf
        liftJSM (onMouseUp t >> render)

    void $ on del click $ do
        t <- now perf
        liftJSM (onDel t >> render)

    void $ on save click $ liftJSM $ do
        t <- now perf
        onSave t >>= \case
            Nothing -> pure ()
            Just (filename, toDraw) -> saveToPNG toShader toDraw filename

    void $ on anim click $ do
        t <- now perf
        liftJSM (onAnim t >> render)

    void $ on tut click $ do
        t <- now perf
        liftJSM (onTut t >> render)

    checkResize <- autoResizeCanvas canvas $ \ pos -> do
        t <- now perf
        onResize t pos >> render
    -- Wish I could use onResize on body, but that does not work somehow
    let regularlyCheckSize = do
          checkResize
          void $ inAnimationFrame' (const regularlyCheckSize)
    regularlyCheckSize -- should trigger the initial render as well


touchOffsetXY :: IsElement e => e -> EventM t TouchEvent (Double, Double)
touchOffsetXY e = do
    touches <- event >>= TouchEvent.getChangedTouches
    touch <- TouchList.item touches 0
    x <- Touch.getPageX touch
    y <- Touch.getPageY touch
    rect <- getBoundingClientRect e
    rx <- DOMRect.getX rect
    ry <- DOMRect.getY rect
    return (fromIntegral x - rx,fromIntegral y - ry)




html :: T.Text
html = T.unlines
    [ "<html>"
    , " <head>"
    , "  <style>" <> css <> "</style>"
    , "  <title>Kaleidogen</title>"
    , " </head>"
    , " <body>"
    , "  <div align='center'>"
    -- Avoid whitespace between the buttons. Stupid HTML.
    , "   <div class='toolbar'>" <>
          "<a id='anim'>🎬</a>" <>
          "<a id='save'>💾</a>" <>
          "<a id='delete'>🗑</a>" <>
          "<a id='tut'>❓</a>"
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
    , "  background-color:black;"
    , "}"
    , ".toolbar {"
    , "  height:min(10vh, 20vw);"
    , "  width:95vw;"
    , "  margin:0;"
    , "  padding:0;"
    , "}"
    , ".toolbar a {"
    , "  display:inline-block;"
    , "  margin: min(1vh,2vw) min(1vh,2vw);"
    , "  border:none;"
    , "  padding:min(.5vh, 1vw);"
    , "  font-size:min(6vh, 12vw);"
    , "  width:min(7vh, 14vw);"
    , "  height:min(7vh, 14vw);"
    , "  background-color:lightblue;"
    , "  border-radius: min(1vh,2vw) min(1vh,2vw);"
    , "  cursor:pointer;"
    , "}"
    , ".toolbar a.hidden {"
    , "  display:none;"
    , "}"
    , ".toolbar a.disabled {"
    , "  background-color:grey;"
    , "  color:white;"
    , "  cursor:default;"
    , "}"
    , ".toolbar a.progress {"
    , "  animation: pulse 2s infinite;"
    , "}"
    , "@keyframes pulse {"
    , "  0% { background-color: lightblue; }"
    , "  50% { background-color: grey; }"
    , "}"
    , "canvas {"
    , "  height:calc(100vh - min(10vh,20vw) - 2.5vw);"
    , "  width:95%;"
    , "  margin:0;"
    , "  padding:0;"
    , "  background-color:white;"
    , "}"
    , "@supports (-webkit-box-reflect:unset) {"
    , "  canvas {"
    , "    height:calc(100vh - 10vh - 2.5vw - 112px);"
    , "  }"
    , "}"
    ]

