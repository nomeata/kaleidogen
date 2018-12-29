{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
import Data.IORef
import Control.Monad.IO.Class
import Data.Bifunctor
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
import Layout hiding (Element)
import qualified CanvasSave
import Animate
import Logic

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

layoutState :: AppState -> ([((DNA, Double), PosAndScale)], ClickFun (Either () Int))
layoutState as@AppState{..} = (toDraw, locate)
  where mainDNA = preview as
        (toPos, locate) = layout (length dnas) canvasSize
        toDraw =
            [ ((dna, 0), toPos (Left ())) | dna <- toList mainDNA ] ++
            zipWith
                (\n (dna,_) -> ((dna, if n `S2.member` sel then 2 else 1), toPos (Right n)))
                [0..] dnas

main :: IO ()
main = run $ do
    doc <- currentDocumentUnchecked
    docEl <- getDocumentElementUnchecked doc
    setInnerHTML docEl html
    CanvasSave.register

    seed0 <- liftIO getRandom
    s <- liftIO $ newIORef (initialAppState seed0)

    canvas <- getElementByIdUnsafe doc ("canvas" :: Text) >>= unsafeCastTo HTMLCanvasElement
    save <- getElementByIdUnsafe doc ("save" :: Text) >>= unsafeCastTo HTMLAnchorElement
    del <- getElementByIdUnsafe doc ("delete" :: Text) >>= unsafeCastTo HTMLAnchorElement

    drawOnCanvas <- shaderCanvas (toFragmentShader . dna2rna) canvas
    drawAnimated <- Animate.interpolate fst 200 (drawOnCanvas . reorderExtraData)

    let render = liftIO (readIORef s) >>= \as@AppState{..} -> do
        let cls :: Text = if S2.isOneSelected sel then "" else "hidden"
        setClassName save cls
        setClassName del cls
        let (toDraw, _locate) = layoutState as
        drawAnimated toDraw

    Just win <- currentWindow
    perf <- getPerformance win
    let handeEvent e = do
        t <- now perf
        as <- liftIO (readIORef s)
        liftIO $ writeIORef s $ handle as e t
        liftJSM render

    _ <- on canvas click $ liftIO (readIORef s) >>= \as@AppState{..} -> do
        pos <- bimap fromIntegral fromIntegral <$> mouseOffsetXY
        case snd (layoutState as) pos of
            Nothing -> return ()
            Just (Left ()) -> handeEvent ClickMain
            Just (Right n) -> handeEvent (ClickSmall n)
    _ <- on del click $ handeEvent Delete
    _ <- on save click $ do
        as <- liftIO (readIORef s)
        for_ (selectedDNA as) $ \dna -> do
            let toDraw = reorderExtraData
                    [ ((dna,0), fst (layoutFullCirlce (1000, 1000)) ()) ]
            saveToPNG (toFragmentShader . dna2rna) toDraw (toFilename dna)

    let canvasResized size = do
        liftIO $ modifyIORef' s (\as -> as { canvasSize = size })
        render
    checkResize <- autoResizeCanvas canvas canvasResized

    -- Wish I could use onResize on body, but that does not work somehow
    let regularlyCheckSize = do
        checkResize
        () <$ inAnimationFrame' (const regularlyCheckSize)
    regularlyCheckSize -- should trigger the initial render as well
    return ()

layout :: Int -> Layout (Either () Int)
layout count = layoutCombined
  where
    layoutTop = layoutFullCirlce
    layoutBottom = layoutFullCirlce
    layoutCombined = layoutTop `layoutAbove` layoutGrid count layoutBottom

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

