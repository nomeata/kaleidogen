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
import qualified Data.Text.Encoding as T
import Data.Bifunctor
import Data.Functor
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random.Strict (getRandom)
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy as BS
import Codec.Picture (encodeBitmap)
import Data.IORef

import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLLinkElement
import GHCJS.DOM.Document hiding (getLocation)
import GHCJS.DOM.Window (getLocation, getLocalStorage)
import GHCJS.DOM.Performance
import GHCJS.DOM.Location (getHash)
import GHCJS.DOM.GlobalPerformance
import GHCJS.DOM.Storage (getItem, setItem)
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers (click, mouseDown, mouseMove, mouseUp, mouseLeave, touchStart, touchMove, touchEnd)
import qualified GHCJS.DOM.TouchEvent as TouchEvent
import qualified GHCJS.DOM.TouchList as TouchList
import qualified GHCJS.DOM.Touch as Touch
import qualified GHCJS.DOM.DOMRect as DOMRect

import ShaderCanvas
#ifndef NoSave
import qualified CanvasSave
#endif
import qualified Animate
import MainProgram
import ProgramScript
import Program
import RunWidget
import Expression
import Img
import DNA

main :: IO ()
main = runWidget mainWidget

mainWidget :: JSM ()
-- mainWidget = runInBrowser renderGraphic mainProgram
-- mainWidget = runInBrowser renderGraphic tutorialProgram
mainWidget = runInBrowser $
    switchProgram mainProgram (scriptProgram mainProgram)


-- Caches the rendered Favicons
faviconUpdater :: JSM (DNA -> JSM ())
faviconUpdater = do
    current <- liftIO $ newIORef []
    doc <- currentDocumentUnchecked
    favicon <- getElementByIdUnsafe doc ("favicon" :: Text) >>= unsafeCastTo HTMLLinkElement

    pure $ \dna -> do
        old <- liftIO $ readIORef current
        unless (dna == old) $ do
            let url = dna2Url dna
            setHref favicon url
            liftIO $ writeIORef current dna

  where
    dna2Url :: DNA -> T.Text
    dna2Url dna = ("data:image/bmp;base64," <>) $ T.decodeUtf8 $ BS.toStrict $
        Base64.encode $ encodeBitmap $ img2Juicy 32 $ toImg $ dna2rna dna


runInBrowser :: ProgramRunner JSM
runInBrowser go = do
    doc <- currentDocumentUnchecked
    docEl <- getDocumentElementUnchecked doc
    setInnerHTML docEl html

#ifndef NoSave
    CanvasSave.register
#endif

    Just win <- currentWindow
    perf <- getPerformance win

    -- Get Dom elements
    canvas <- getElementByIdUnsafe doc ("canvas" :: Text) >>= unsafeCastTo HTMLCanvasElement
#ifndef NoSave
    save <- getElementByIdUnsafe doc ("save" :: Text) >>= unsafeCastTo HTMLAnchorElement
#endif
    anim <- getElementByIdUnsafe doc ("anim" :: Text) >>= unsafeCastTo HTMLAnchorElement
    del <- getElementByIdUnsafe doc ("delete" :: Text) >>= unsafeCastTo HTMLAnchorElement
    reset <- getElementByIdUnsafe doc ("reset" :: Text) >>= unsafeCastTo HTMLAnchorElement
    tut <- getElementByIdUnsafe doc ("tut" :: Text) >>= unsafeCastTo HTMLAnchorElement

    setFavicon <- faviconUpdater

    drawShaderCircles <- shaderCanvas canvas

    loc <- getLocation win
    -- This does not work inside the Telegram app, it seems
    isTelegram <- ("tgShareScoreUrl" `T.isInfixOf`) <$> getHash loc

    let confButton e True  _     = setClassName e ("progress"::Text)
        confButton e False True  = setClassName e (""::Text)
        confButton e False False = setClassName e ("disabled"::Text)

    -- Get previous state
    storage <- currentWindowUnchecked >>= getLocalStorage
    old_state <- getItem storage ("logic_state"::String)

    size0 <- querySize canvas
    t0 <- now perf
    seed0 <- liftIO getRandom

    Callbacks{..} <- go old_state seed0 t0 size0

    render <- Animate.animate $ \_ -> do
        t <- now perf
        DrawResult {..} <- onDraw t
        drawShaderCircles objects
        confButton del  False canDelete
#ifndef NoSave
        confButton save False (saveSupported && not isTelegram && canSave)
#endif
        confButton anim animInProgress canAnim
        confButton reset False  canReset
        confButton tut  tutInProgress  True

        -- favicon
        setFavicon mainDNA

        return stillAnimating

    let store_and_render = do
          onSerialize >>= setItem storage ("logic_state"::String)
          render

    void $ on canvas mouseDown $ do
        t <- now perf
        pos <- bimap fromIntegral fromIntegral <$> mouseOffsetXY
        liftJSM (onMouseDown t pos >> store_and_render)

    void $ on canvas mouseMove $ do
        t <- now perf
        pos <- bimap fromIntegral fromIntegral <$> mouseOffsetXY
        liftJSM (onMove t pos >> store_and_render)

    void $ on canvas mouseUp $ do
        t <- now perf
        liftJSM (onMouseUp t >> store_and_render)

    void $ on canvas mouseLeave $ do
        t <- now perf
        liftJSM (onMouseOut t >> store_and_render)

    void $ on canvas touchStart $ do
        t <- now perf
        pos <- touchOffsetXY canvas
        liftJSM (onMouseDown t pos >> store_and_render)
        preventDefault

    void $ on canvas touchMove $ do
        t <- now perf
        pos <- touchOffsetXY canvas
        liftJSM (onMove t pos >> store_and_render)

    void $ on canvas touchEnd $ do
        t <- now perf
        liftJSM (onMouseUp t >> store_and_render)

    void $ on del click $ do
        t <- now perf
        liftJSM (onDel t >> store_and_render)

#ifndef NoSave
    void $ on save click $ liftJSM $ do
        t <- now perf
        onSave t >>= \case
            Nothing -> pure ()
            Just (filename, toDraw) -> saveToPNG toDraw filename
#endif

    void $ on anim click $ do
        t <- now perf
        liftJSM (onAnim t >> render)

    void $ on reset click $ do
        t <- now perf
        seed <- liftIO getRandom
        liftJSM (onReset t seed >> store_and_render)

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
    , "  <link id='favicon' rel='icon' href='' />"
    , " </head>"
    , " <body>"
    -- Avoid whitespace between the buttons. Stupid HTML.
    , "   <div class='toolbar'>" <>
          "<a id='anim'>🎬</a>" <>
#ifndef NoSave
          "<a id='save'>💾</a>" <>
#endif
          "<a id='delete'>🗑</a>" <>
          "<a id='reset'>\129533</a>" <>
          "<a id='tut'>❓</a>"
    ,     "</div>"
    , "   <canvas id='canvas'></canvas>"
    , " </body>"
    , "</html>"
    ]

css :: T.Text
css = T.unlines
    [ "html {"
    , "  margin: 0;"
    , "  padding: 0;"
    , "  height: 100%;"
    , "  width: 100%;"
    , "}"
    , "body {"
    , "  display: flex;"
    , "  margin: 0;"
    , "  padding: 0;"
    , "  height: 100%;"
    , "  width: 100%;"
    , "  position: fixed;"
    , "  flex-direction: column;"
    , "  align-itmes: strech;"
    , "  background-color:black;"
    , "}"
    , ".toolbar {"
    , "  height:18vmin;"
    , "  width:95vw;"
    , "  margin:0;"
    , "  padding:0;"
    , "  text-align:center;"
    , "}"
    , ".toolbar a {"
    , "  display:inline-block;"
    , "  margin: 2vmin 2vmin;"
    , "  border:none;"
    , "  padding:1vmin;"
    , "  font-size:10vmin;"
    , "  width:12vmin;"
    , "  height:12vmin;"
    , "  background-color:lightblue;"
    , "  border-radius: 2vmin;"
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
    , "  width:100%;"
    , "  height:10%;"
    , "  margin:auto;"
    , "  padding:0;"
    , "  background-color:white;"
    , "  flex-grow: 1;"
    , "  flex-basis: fit-content;"
    , "}"
    , "@supports (-webkit-box-reflect:unset) {"
    , "  canvas {"
    , "  }"
    , "}"
    ]

