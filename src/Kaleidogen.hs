{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
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
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor

import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.Document
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers (resize, click)

import ShaderCanvas
import Expression
import GLSL
import DNA
import qualified SelectTwo as S2
import Layout hiding (Element)
import qualified CanvasSave
-- import Animate

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


{-
patternCanvasLayout :: (MonadWidget t m, MonadJSM (Performable m)) =>
    Event t () ->
    Layout a (DNA, Double) c ->
    Morpher m t (DNA, Double) ->
    Dynamic t a ->
    m (Event t c)
patternCanvasLayout eSizeMayChange layout morpher dData = mdo
    (dClick, dSize) <- shaderCanvas
        (toFragmentShader . dna2rna)
        eSizeMayChange
        (reorderExtraData <$> dUniqued)
    let dLaidOut = layout <$> dData <*> dSize
    let dLocate = snd <$> dLaidOut
    let eSelectOne = fmapMaybe id $ current dLocate <@> dClick
    dMorphed <- morpher (fst <$> dLaidOut)
    dUniqued <- holdUniqDyn dMorphed
    return eSelectOne
-}

reorderExtraData :: [((DNA, a), (b,c), d)] -> [(DNA, (a, b, c, d))]
reorderExtraData = map $ \((d,b), (x,y), s) -> (d, (b, x, y, s))

toFilename :: DNA -> T.Text
toFilename dna = "kaleidogen-" <> dna2hex dna <> ".png"

type Seed = Int

preview :: Seed -> S2.SelectTwo DNA -> Maybe DNA
preview _    S2.NoneSelected = Nothing
preview _    (S2.OneSelected x)   = Just x
preview seed (S2.TwoSelected x y) = Just $ crossover seed x y

data AppState = AppState
    { seed :: Seed
    , canvasSize :: (Double, Double)
    , dnas :: [DNA]
    , sel :: S2.SelectTwo Int
    }

initialAppState :: Seed -> AppState
initialAppState seed = AppState {..}
  where
    canvasSize = (1000, 1000)
    dnas = initialDNAs
    sel = S2.duolton 0 1

layoutState :: AppState -> ([((DNA, Double), (Double, Double), Double)], (Double, Double) -> Maybe (Either () (Int, ())))
layoutState AppState{..} = (toDraw, locate)
  where selectedTwo = (dnas!!) <$> sel
        mainDNA = preview seed selectedTwo
        withSelection = zip dnas (map (`S2.member` sel) [0..])
        (toDraw, locate) = layout (mainDNA, withSelection) canvasSize

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

    let render = liftIO (readIORef s) >>= \as@AppState{..} -> do
        let cls :: Text = if S2.isOneSelected sel then "" else "hidden"
        setClassName save cls
        setClassName del cls
        let (toDraw, _locate) = layoutState as
        drawOnCanvas (reorderExtraData toDraw)

    _ <- on canvas click $ liftIO (readIORef s) >>= \as@AppState{..} -> do
        pos <- bimap fromIntegral fromIntegral <$> mouseOffsetXY
        case snd (layoutState as) pos of
            Nothing -> return ()
            Just (Left ()) -> do
                let selectedTwo = (dnas!!) <$> sel
                case preview seed selectedTwo of
                    Nothing -> return ()
                    Just new -> unless (new `elem` dnas) $ do
                        liftIO $ writeIORef s as
                            { sel = S2.empty
                            , dnas = dnas ++ [new]
                            }
                        liftJSM render
            Just (Right (n, ())) -> do
                let sel' = S2.flip sel n
                liftIO $ writeIORef s $ as { sel = sel' }
                liftJSM render
        return ()
    _ <- on del click $ liftIO (readIORef s) >>= \as@AppState{..} ->
        case sel of
            S2.OneSelected n -> do
                liftIO $ writeIORef s $ as
                    { dnas = take n dnas ++ drop (n+1) dnas
                    , sel = S2.empty }
                liftJSM render
            _ -> return ()
    _ <- on save click $ liftIO (readIORef s) >>= \AppState{..} ->
        case sel of
            S2.OneSelected n -> do
                let dna = dnas !! n
                let toDraw = reorderExtraData $ fst $ layoutFullCirlce (dna, 0) (1000, 1000)
                liftIO $ putStrLn "Foo"
                saveToPNG
                    (toFragmentShader . dna2rna)
                    toDraw
                    (toFilename dna)
                liftIO $ putStrLn "Bar"
            _ -> return ()

    let canvasResized size = do
        liftIO $ modifyIORef' s (\as -> as { canvasSize = size })
        render
    checkResize <- autoResizeCanvas canvas canvasResized

    body <- getBodyUnchecked doc
    _ <- on body resize $ do
        liftIO $ putStrLn "onResize"
        liftJSM checkResize

    checkResize -- should trigger the initial render as well
    return ()

layout :: Layout (Maybe t, [(t, Bool)]) (t, Double) (Either () (Int, ()))
layout = layoutCombined
  where
    layoutTop = layoutMaybe $ mapLayout (,0) layoutFullCirlce
    layoutBottom = mapLayout (\(d,b)-> (d,if b then 2 else 1)) layoutFullCirlce
    layoutCombined = layoutTop `layoutAbove` layoutGrid layoutBottom


  {-
  mainWidgetWithHead htmlHead $
    elAttr "div" ("align" =: "center") $ mdo
        -- We could exchange this for a resize event on the window
        dAnimationFrame <- getAnimationFrameD
        let eSizeMayChange = () <$ updated dAnimationFrame

        (eDelete, eSave) <- divClass "toolbar" $
            (,) <$>
            -- toolbarButton "➕" dCanAdd <*>
            toolbarButton "🗑" dCanDel <*>
            toolbarButton "💾" dCanSave

        let layoutTop = layoutMaybe $ mapLayout (,0) layoutFullCirlce
        let layoutBottom = mapLayout (\(dnap,b)-> (dnap,if b then 2 else 1)) layoutFullCirlce
        let layoutCombined = layoutTop `layoutAbove` layoutGrid layoutBottom

        let morpher = interpolate fst 200 dAnimationFrame

        eAdded2_SelectOne <-
            patternCanvasLayout eSizeMayChange layoutCombined morpher $
                (,) <$> dMainGenome <*> dGenomesWithSelection
        let (eAdded2, eSelectOne) = fanEither eAdded2_SelectOne

        let eAdded = gate (current dCanAdd) eAdded2
        let dFilename = toFilename <$> dMainGenome
        let eSaveAs = tag (current dFilename) eSave

        let dForSave = reorderExtraData . maybe [] (\x -> fst (layoutFullCirlce (x, 0) (1000, 1000))) <$> dMainGenome
        performEvent_ (saveToPNG (toFragmentShader . dna2rna) <$> current dForSave <@> eSaveAs)

        let dCanAdd =
                (\new xs -> maybe False (`notElem` xs) new) <$>
                dMainGenome <*> dGenomes
        let dCanDel = S2.isOneSelected <$> dPairSelected
        let dCanSave = S2.isOneSelected <$> dPairSelected

        let eClear = eAdded <> eDelete

        (dPairSelected, dGenomesWithSelection)
            <- selectTwoInteraction eClear (fst <$> eSelectOne) dGenomes

        let dMainGenome = preview seed <$> dPairSelected

        dGenomes <- stateMachine initialDNAs
            [ (\new xs -> xs ++ [new]) <$>
                    fmapMaybe id (tag (current dMainGenome) eAdded)
            , delete <$> fmapMaybe id (tag (current dMainGenome) eDelete)
            ]
        return ()
    -}

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

