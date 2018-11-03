{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Kaleidogen where

import Control.Monad
import qualified Data.Text as T
import Data.Monoid
import Data.List
import Data.Function
import Control.Monad.Fix

import Reflex.Dom

import ShaderCanvas
-- import CanvasSave
import Expression
import GLSL
import DNA
import qualified SelectTwo as S2
import Layout

import Language.Javascript.JSaddle.Types (MonadJSM, liftJSM, JSM)


performD :: (MonadHold t m, PostBuild t m, PerformEvent t m) =>
    (a -> Performable m b) -> Dynamic t a -> m (Dynamic t (Maybe b))
performD f d = do
    pb <- getPostBuild
    e1 <- performEvent (f <$> updated d)
    e2 <- performEvent (f <$> current d <@ pb)
    holdDyn Nothing $ Just <$> leftmost [ e1, e2 ]

stateMachine :: (MonadFix m, MonadHold t m, Reflex t) =>
    a -> [Event t (a -> a)] -> m (Dynamic t a)
stateMachine x es = foldDyn ($) x $ mergeWith (.) es

type CompileFun = T.Text -> JSM (Maybe CompiledProgram)
type DNAP = (DNA, Maybe CompiledProgram, Maybe CompiledProgram)
getDNA :: DNAP -> DNA
getDNA (x,_,_) = x

patternCanvasLayout :: (MonadWidget t m, MonadJSM (Performable m)) =>
    Layout a (Maybe CompiledProgram, Double) c ->
    Dynamic t a ->
    m (Event t c, CompileFun)
patternCanvasLayout Layout{..} dData = mdo
    (dClick, dSize, compile) <- shaderCanvas dLaidOut
    let eSelectOne = fmapMaybe id $ locate <$> current dSize <*> current dData <@> dClick
    let dLaidOut = layout <$> dSize <*> dData
    return (eSelectOne, compile)

patternCanvasMay :: (MonadWidget t m, MonadJSM (Performable m)) =>
    Dynamic t (Maybe DNAP) -> m (Event t (), CompileFun)
patternCanvasMay dGenome = do
    -- let showTitle dna = T.pack $ unlines [dna2hex dna, show (dna2rna dna)]
    let showTitle = maybe "" dna2hex
    let layout = layoutMaybe $ mapLayout (\(_,x,_)-> (x,0)) (layoutLarge 1)
    elDynAttr "div" ((\dna -> "title" =: showTitle (fmap getDNA dna)) <$> dGenome) $ mdo
        patternCanvasLayout layout dGenome

patternCanvasList :: (MonadWidget t m, MonadJSM (Performable m)) =>
    Dynamic t [(DNAP, Bool)] ->
    m (Event t Int, CompileFun)
patternCanvasList dGenomes = mdo
    let layout = layoutGrid $ mapLayout (\((_,_,x),b)-> (x,if b then 1 else 0)) (layoutLarge 0.9)
    (eClick, compile) <- patternCanvasLayout layout dGenomes
    return (fst <$> eClick, compile)

selectTwoInteraction :: forall t m a.
    (MonadFix m, Reflex t, MonadHold t m) =>
    Event t () -> Event t Int -> Dynamic t [a] ->
    m (Dynamic t (S2.SelectTwo a), Dynamic t [(a,Bool)])
selectTwoInteraction eClear eSelectOne dData = mdo
    let eSelection :: Event t (S2.SelectTwo Int) =
            attachWith S2.flip (current dSelected) eSelectOne
    -- This separation is necessary so that eClear may depend on the eSelectedN
    -- that we return; otherwise we get a loop
    let eClearedSelection :: Event t (S2.SelectTwo Int)
            = leftmost [S2.empty <$ eClear, eSelection]
    dSelected :: Dynamic t (S2.SelectTwo Int)
            <- holdDyn (S2.singleton 0) eClearedSelection

    let dDataWithSelection =
            (\xs s -> zip xs (map (`S2.member` s) [0..])) <$> dData <*> dSelected

    dSelection' <- holdDyn (S2.singleton 0) $ leftmost [ eSelection, S2.empty <$ eClear]
    let dSelectedData = (\xs s -> fmap (xs!!) s) <$> dData <*> dSelection'
    return (dSelectedData, dDataWithSelection)

toFilename :: Maybe DNA -> T.Text
toFilename (Just dna) = "kaleidogen-" <> dna2hex dna <> ".png"
toFilename Nothing    = "error.png"

type Seed = Int

preview :: Seed -> S2.SelectTwo DNA -> Maybe DNA
preview _    S2.NoneSelected = Nothing
preview _    (S2.OneSelected x)   = Just x
preview seed (S2.TwoSelected x y) = Just $ crossover seed x y

toDNAP :: (CompileFun, CompileFun) -> DNA -> JSM DNAP
toDNAP (compile1, compile2) x = do
    let t = T.pack $ toFragmentShader $ dna2rna x
    p1 <- compile1 t
    p2 <- compile2 t
    return (x,p1,p2)

previewPGM :: Seed -> (CompileFun, CompileFun) -> S2.SelectTwo DNAP -> JSM (Maybe DNAP)
previewPGM _ _ S2.NoneSelected = return Nothing
previewPGM _ _ (S2.OneSelected x) = return $ Just x
previewPGM seed cfs (S2.TwoSelected (x,_,_) (y,_,_)) = do
    let z = crossover seed x y
    Just <$> toDNAP cfs z

toolbarButton :: (DomBuilder t m, PostBuild t m) =>
    T.Text -> Dynamic t Bool -> m (Event t ())
toolbarButton txt dActive = do
    let dAttrs = (\case True -> mempty; False -> "class" =: "disabled") <$> dActive
    (e,_) <- elDynAttr' "a" dAttrs (text txt)
    return $ gate (current dActive) $ domEvent Click e

main :: IO ()
main = do
  seed <- getRandom
  mainWidgetWithHead htmlHead $
    elAttr "div" ("align" =: "center") $ mdo
        (eAdded1, eDelete) <- divClass "toolbar" $
            (,) <$>
            toolbarButton "➕" dCanAdd <*>
            toolbarButton "🗑" dCanDel

        (eAdded2, compile1) <- divClass "new-pat" $
            patternCanvasMay dMainGenome

        (eSelectOne, compile2) <- divClass "patterns" $
            patternCanvasList dGenomesWithSelection

        let eAdded = eAdded1 <> gate (current dCanAdd) eAdded2
        -- let dFilename = toFilename . fmap getDNA <$> dMainGenome
        -- let eSaveAs = tag (current dFilename) eSave

        let dCanAdd =
                (\new xs -> maybe False (`notElem` map getDNA xs) (getDNA <$> new)) <$>
                dMainGenome <*> dGenomes
        let dCanDel =
                (\new xs -> maybe False (`elem`    map getDNA xs) (getDNA <$> new)) <$>
                dMainGenome <*> dGenomes
        -- let dCanSave = isJust <$> dMainGenome

        let eClear = eAdded <> eDelete

        (dPairSelected, dGenomesWithSelection)
            <- selectTwoInteraction eClear eSelectOne dGenomes

        let cfs = (compile1, compile2)
        dMainGenome <- fmap join <$> performD (liftJSM . previewPGM seed cfs) dPairSelected

        initialDNAPs <- liftJSM $ mapM (toDNAP cfs) initialDNAs
        dGenomes <- stateMachine initialDNAPs
            [ (\new xs -> xs ++ [new]) <$>
                    fmapMaybe id (tag (current dMainGenome) eAdded)
            , deleteBy ((==) `on` getDNA)  <$> fmapMaybe id (tag (current dMainGenome) eDelete)
            ]
        return ()
  where
    htmlHead :: DomBuilder t m => m ()
    htmlHead = do
        el "style" (text css)
        el "title" (text "Kaleidogen")
        elAttr "script" ("src" =: "https://fastcdn.org/FileSaver.js/1.1.20151003/FileSaver.min.js") (return ())

css :: T.Text
css = T.unlines
    [ "html {"
    , "  margin: 0;"
    , "  height: 100%;"
    , "}"
    , "body {"
    , "  display: flex;"
    , "  margin: 0;"
    , "  height: 100%;"
    , "  flex-direction: column;"
    , "}"
    , ".toolbar {"
    , "  height:10vh;"
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
    , ".new-pat canvas {"
    , "  height:45vh;"
    , "  width:45vh;"
    , "  margin:0;"
    , "}"
    , ".patterns canvas {"
    , "  margin:0;"
    , "  height:40vh;"
    , "  width:100%;"
    , "}"
    ]

