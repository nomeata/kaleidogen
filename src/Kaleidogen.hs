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
import qualified Data.Map as M
import Data.Monoid
import Data.IORef
import Data.Bifunctor
import Control.Monad.IO.Class
import Data.Foldable

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
import Expression
import GLSL
import DNA
import qualified SelectTwo as S2
import Layout
import qualified CanvasSave
import Animate
import Logic
import qualified Presentation

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

reorderExtraData :: [((DNA, a), ((b,c),d))] -> [(DNA, (a, b, c, d))]
reorderExtraData = map $ \((d,b),((x,y),s)) -> (d, (b, x, y, s))

toFilename :: DNA -> T.Text
toFilename dna = "kaleidogen-" <> dna2hex dna <> ".png"

layoutFun :: (Double, Double) -> AbstractPos -> PosAndScale
layoutFun size MainPos
    = topHalf layoutFullCirlce size ()
layoutFun size (SmallPos c n)
    = bottomHalf (layoutGrid False c) size n
layoutFun size (DeletedPos c n)
    = bottomHalf (layoutGrid True c) size n

getLayoutFun :: IORef (Double, Double) -> IO Presentation.LayoutFun
getLayoutFun r = do
    size <- readIORef r
    return (layoutFun size)

main :: IO ()
main = run $ do
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

    -- Set up global state
    seed0 <- liftIO getRandom
    let as0 = initialAppState seed0
    asRef <- liftIO $ newIORef as0
    size0 <- querySize canvas
    sizeRef <- liftIO $ newIORef size0
    pRef <- liftIO Presentation.initRef
    let handleCmds cs = do
        t <- now perf
        lf <- liftIO $ getLayoutFun sizeRef
        liftIO $ Presentation.handleCmdsRef t lf cs pRef
    handleCmds (initialCommands as0)

    drawOnCanvas <- shaderCanvas (toFragmentShader . dna2rna) canvas
    let draw t = do
        as <- liftIO $ readIORef asRef
        (p, continue) <- liftIO (Presentation.presentAtRef t (isSelected as) pRef)
        drawOnCanvas [ (key2dna k, (e,x,y,s)) | (k,(e,((x,y),s))) <- M.toList p ]
        return continue
    drawAnimated <- Animate.animate draw

    let render = liftIO (readIORef asRef) >>= \AppState{..} -> do
        let cls :: Text = if S2.isOneSelected sel then "" else "hidden"
        setClassName save cls
        setClassName del cls
        drawAnimated

    let handeEvent e = do
        as <- liftIO (readIORef asRef)
        let (as', cs) = handle as e
        liftIO $ writeIORef asRef as'
        liftJSM $ handleCmds cs
        liftJSM render

    _ <- on canvas click $ do
        t <- now perf
        as@AppState{..} <- liftIO (readIORef asRef)
        (p, _continue) <- liftIO (Presentation.presentAtRef t (isSelected as) pRef)
        pos <- bimap fromIntegral fromIntegral <$> mouseOffsetXY
        case Presentation.locateClick p pos of
            Just k -> handeEvent (Click k)
            Nothing -> return ()
    _ <- on del click $ handeEvent Delete
    _ <- on save click $ do
        as <- liftIO (readIORef asRef)
        for_ (selectedDNA as) $ \dna -> do
            let toDraw = reorderExtraData
                    [ ((dna,0), layoutFullCirlce (1000, 1000) ()) ]
            saveToPNG (toFragmentShader . dna2rna) toDraw (toFilename dna)

    let canvasResized size = do
        liftIO $ writeIORef sizeRef size
        as <- liftIO $ readIORef asRef
        handleCmds (initialCommands as)
        render
    checkResize <- autoResizeCanvas canvas canvasResized

    -- Wish I could use onResize on body, but that does not work somehow
    let regularlyCheckSize = do
        checkResize
        () <$ inAnimationFrame' (const regularlyCheckSize)
    regularlyCheckSize -- should trigger the initial render as well
    return ()

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

