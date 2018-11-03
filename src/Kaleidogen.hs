{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
import AnimationFrame
-- import CanvasSave
import Expression
import GLSL
import DNA
import qualified SelectTwo as S2
import Layout
import Animate

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

patternCanvasLayout :: (MonadWidget t m, MonadJSM (Performable m)) =>
    Event t () ->
    Layout a (DNAP, Double) c ->
    Morpher m t (DNAP, Double) ->
    Dynamic t a ->
    m (Event t c, CompileFun)
patternCanvasLayout eSizeMayChange Layout{..} morpher dData = mdo
    (dClick, dSize, compile) <- shaderCanvas eSizeMayChange (getProgramD dMorphed)
    let eSelectOne = fmapMaybe id $ locate <$> current dSize <*> current dData <@> dClick
    let dLaidOut = layout <$> dSize <*> dData
    dMorphed <- morpher dLaidOut
    return (eSelectOne, compile)


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

type CompileFun = T.Text -> JSM (Maybe CompiledProgram)

data DNAP = DNAP DNA (Maybe CompiledProgram)
instance Eq DNAP where (==) = (==) `on` getDNA
instance Ord DNAP where compare = compare `on` getDNA

getDNA :: DNAP -> DNA
getDNA (DNAP x _) = x
getProgram :: DNAP -> Maybe CompiledProgram
getProgram (DNAP _ p) = p

toDNAP :: CompileFun -> DNA -> JSM DNAP
toDNAP compile x = do
    let t = T.pack $ toFragmentShader $ dna2rna x
    p <- compile t
    return $ DNAP x p

getProgramD :: Functor f => f [((DNAP, a), b, c)] -> f [((Maybe CompiledProgram, a), b, c)]
getProgramD = fmap $ map (\((x,b), p, s) -> ((getProgram x, b), p, s))

toFilename :: Maybe DNA -> T.Text
toFilename (Just dna) = "kaleidogen-" <> dna2hex dna <> ".png"
toFilename Nothing    = "error.png"

type Seed = Int

preview :: Seed -> S2.SelectTwo DNA -> Maybe DNA
preview _    S2.NoneSelected = Nothing
preview _    (S2.OneSelected x)   = Just x
preview seed (S2.TwoSelected x y) = Just $ crossover seed x y


previewPGM :: Seed -> CompileFun -> S2.SelectTwo DNAP -> JSM (Maybe DNAP)
previewPGM _ _ S2.NoneSelected = return Nothing
previewPGM _ _ (S2.OneSelected x) = return $ Just x
previewPGM seed compile (S2.TwoSelected x y) = do
    let z = crossover seed (getDNA x) (getDNA y)
    Just <$> toDNAP compile z

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
        -- We could exchange this for a resize event on the window
        dAnimationFrame <- getAnimationFrameD
        let eSizeMayChange = () <$ updated dAnimationFrame

        (eAdded1, eDelete) <- divClass "toolbar" $
            (,) <$>
            toolbarButton "➕" dCanAdd <*>
            toolbarButton "🗑" dCanDel

        let layoutTop = layoutMaybe $ mapLayout (,0) (layoutLarge 1)
        let layoutBottom = layoutGrid $ mapLayout (\(dnap,b)-> (dnap,if b then 1 else 0)) (layoutLarge 0.9)
        let layoutCombined = layoutTop `layoutAbove` layoutBottom

        let morpher = interpolate fst 1000 dAnimationFrame

        (eAdded2_SelectOne, compile) <-
            patternCanvasLayout eSizeMayChange layoutCombined morpher $
                (,) <$> dMainGenome <*> dGenomesWithSelection
        let (eAdded2, eSelectOne) = fanEither eAdded2_SelectOne

        let eAdded = eAdded1 <> gate (current dCanAdd) eAdded2
        -- let dFilename = toFilename . fmap getDNA <$> dMainGenome
        -- let eSaveAs = tag (current dFilename) eSave

        let dCanAdd =
                (\new xs -> maybe False (`notElem` xs) new) <$>
                dMainGenome <*> dGenomes
        let dCanDel =
                (\new xs -> maybe False (`elem`    xs) new) <$>
                dMainGenome <*> dGenomes
        -- let dCanSave = isJust <$> dMainGenome

        let eClear = eAdded <> eDelete

        (dPairSelected, dGenomesWithSelection)
            <- selectTwoInteraction eClear (fst <$> eSelectOne) dGenomes

        dMainGenome <- fmap join <$> performD (liftJSM . previewPGM seed compile) dPairSelected

        initialDNAPs <- liftJSM $ mapM (toDNAP compile) initialDNAs
        dGenomes <- stateMachine initialDNAPs
            [ (\new xs -> xs ++ [new]) <$>
                    fmapMaybe id (tag (current dMainGenome) eAdded)
            , delete <$> fmapMaybe id (tag (current dMainGenome) eDelete)
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
    , "  margin:0;"
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
    , "  height:90vh;"
    , "  width:100vw;"
    , "  margin:0;"
    , "}"
    ]

