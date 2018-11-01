{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Kaleidogen where

import ShaderCanvas
import Reflex.Dom
import CanvasSave

import qualified Data.Text as T
import Data.Bifunctor
import Data.Monoid
import Data.List
import Data.Maybe
import Control.Monad.Fix

import Expression
import GLSL
import DNA
import qualified SBNL as S

stateMachine :: (MonadFix m, MonadHold t m, Reflex t) =>
    a -> [Event t (a -> a)] -> m (Dynamic t a)
stateMachine x es = foldDyn ($) x $ mergeWith (.) es

selectNList ::
    forall t a b m.
    (Adjustable t m, DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m, Ord a) =>
    Int ->
    [a] ->
    Event t () ->
    Dynamic t [a] ->
    (Dynamic t a -> m b) ->
    m (Event t [a], Dynamic t [b])
selectNList n s0 eClear dxs act = mdo
    -- TODO: This currently only works if the input is is only appended to, but
    -- not if elements is deleted. If we need that, we should update the selected set
    let eSelection :: Event t (S.SBNL a) = attachWith (S.flipMember n) (current dSelected) eClicks
    -- This separation is necessary so that eClear may depend on the eSelectedN
    -- that we return; otherwise we get a loop
    let eClearedSelection :: Event t (S.SBNL a) = leftmost [S.empty <$ eClear, eSelection]
    dSelected :: Dynamic t (S.SBNL a) <- holdDyn s0 eClearedSelection

    let dxs' = (\s -> map (\x -> (x `S.member` s, x))) <$> dSelected <*> dxs
    dstuff <- simpleList dxs' $ \dx' -> do
        enabled <- holdUniqDyn (fst <$> dx')
        dx      <- holdUniqDyn (snd <$> dx')
        let attrs = (\case { True -> "class" =: "selected" ; False -> mempty}) <$> enabled
        (eClick,dy) <- clickable $ elDynAttr' "div" attrs $ act dx
        let eTaggedClick = tag (current dx) eClick
        return (eTaggedClick, dy)
    let eClicks = switch (current (leftmost . fmap fst <$> dstuff))
    let dys = fmap snd <$> dstuff
    return (eSelection, dys)

-- | A div class with an event to make it scroll all the way to the right.
-- | Does not work yet, postposed right now
scrollRightDivClass :: (MonadHold t m, PostBuild t m, DomBuilder t m) => Event t () -> T.Text -> m a -> m a
scrollRightDivClass e cls act = do
    attrs <- holdDyn mempty ("scrollLeft" =: "10000" <$ e)
    let attrs' = ("class" =: cls <>) <$> attrs
    elDynAttr "div" attrs' act

patternCanvans :: MonadWidget t m =>
    Dynamic t DNA -> m (Dynamic t T.Text)
patternCanvans dGenome = patternCanvansMay mempty (Just <$> dGenome)

patternCanvansMay :: MonadWidget t m =>
    Event t T.Text -> Dynamic t (Maybe DNA) -> m (Dynamic t T.Text)
patternCanvansMay eSave dGenome = do
    -- let showTitle dna = T.pack $ unlines [dna2hex dna, show (dna2rna dna)]
    let showTitle = maybe "" dna2hex
    elDynAttr "div" ((\dna -> "title" =: showTitle dna) <$> dGenome) $ mdo
        let dShader = T.pack . maybe blankShader (toFragmentShader . dna2rna) <$> dGenome
        let dDrawable = layoutLarge <$> dSize <*> dShader
        (e,(_dClick, dErr, dSize)) <- shaderCanvas' dDrawable
        _ <- performEvent $ (<$> eSave) $ \name -> CanvasSave.save name e
        return dErr

patternCanvasList :: MonadWidget t m =>
    Dynamic t [(DNA, Bool)] ->
    m (Event t (Int, Int), Dynamic t T.Text, Dynamic t (Double, Double))
patternCanvasList dGenomes = mdo
    let dShaders = map (first (T.pack . toFragmentShader . dna2rna)) <$> dGenomes
    let dDrawable = layoutGrid <$> dSize <*> dShaders
    (dClick, dErr, dSize) <- shaderCanvas dDrawable
    return (dClick, dErr, dSize)

patternCanvasSelectableList :: MonadWidget t m =>
    Dynamic t [DNA] -> m (Dynamic t T.Text)
patternCanvasSelectableList dGenomes = mdo
    (dClick, dErr, dSize) <- patternCanvasList dSelectedGenomes
    let eSelectOne = locateClick <$> current dSize <@> dClick
    dSelected <- foldDyn id S.empty $ mconcat
        [ flip (S.flipMember 2) <$> eSelectOne ]
    let dSelectedGenomes =
            (\xs s -> zip xs (map (`S.member` s) [0..])) <$> dGenomes <*> dSelected
    return dErr

layoutLarge :: (Double, Double) -> a -> [(a, Double, (Double, Double), Double)]
layoutLarge (w, h) x = [(x, 0, (w/2, h/2), min (w/2) (h/2))]

layoutGrid :: (Double, Double) -> [(a, Bool)] -> [(a, Double, (Double, Double), Double)]
layoutGrid (w,h) xs =
    [ (a, if selected then 1 else 0, (x,y), 0.9 * (s/2))
    | (i,(a, selected)) <- zip [0..] xs
    , let x = 0.5 * s + s * fromIntegral (i `mod` per_row)
    , let y = 0.5 * s + s * fromIntegral (i `div` per_row)
    ]
  where
    per_row = floor (w/(h/4)) :: Integer
    s = w/fromIntegral per_row

locateClick :: (Double, Double) -> (Int, Int) -> Int
locateClick _ _ = 0

toFilename :: Maybe DNA -> T.Text
toFilename (Just dna) = "kaleidogen-" <> dna2hex dna <> ".png"
toFilename Nothing    = "error.png"


clickable :: (HasDomEvent t el 'ClickTag, Functor f) =>
   f (el, c) -> f (Event t (DomEventType el 'ClickTag), c)
clickable act = first (domEvent Click) <$> act

divClass' :: DomBuilder t m =>
    T.Text ->
    m a ->
    m (Element EventResult (DomBuilderSpace m) t, a)
divClass' cls = elAttr' "div" ("class" =: cls)


type Seed = Int

preview :: Seed -> [DNA] -> Maybe DNA
preview _    []    = Nothing
preview _    [x]   = Just x
preview seed [x,y] = Just $ crossover seed x y
preview _ _ = Nothing -- Should not be possible

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
        (eAdded1, eDelete, eSave) <- divClass "toolbar" $
            (,,) <$>
            toolbarButton "➕" dCanAdd <*>
            toolbarButton "🗑" dCanDel <*>
            toolbarButton "💾" dCanSave

        (eAdded2, _) <- clickable $ divClass' "new-pat" $
            patternCanvansMay eSaveAs dNewGenome

        let eAdded = eAdded1 <> gate (current dCanAdd) eAdded2
        let dFilename = toFilename <$> dNewGenome
        let eSaveAs = tag (current dFilename) eSave

        let dGenomes = map fst <$> dSelectedGenomes
        let dCanAdd = (\new xs -> maybe False (`notElem` xs) new) <$> dNewGenome <*> dGenomes
        let dCanDel = (\new xs -> maybe False (`elem`    xs) new) <$> dNewGenome <*> dGenomes
        let dCanSave = isJust <$> dNewGenome

        let s0 = take 1 initialDNAs

        {-
        (ePairSelected, _dErrors) <- divClass "patterns" $ do
            selectNList 2 s0 (eAdded <> eDelete) dGenomes $ patternCanvans
        -}
        let ePairSelected = never
        _dErrors <- divClass "patterns" $
            patternCanvasList dSelectedGenomes

        {-
        inp <- textArea $ def
           & textAreaConfig_initialValue .~ "1 2"
           & textAreaConfig_attributes .~ (return ("style"  =: "width:80%"))
           -- & textAreaConfig_setValue .~ updated (T.unlines <$> genomes)
        --let genomes = T.lines <$> _textArea_value inp

        el "pre" $ dynText (T.unlines <$> genomes)
        -}
        dPairSelected <- holdDyn s0 $ mconcat
            [ ePairSelected
            , [] <$ eAdded
            , [] <$ eDelete
            ]

        let dNewGenome = preview seed <$> dPairSelected

        let selectFirst [] = []
            selectFirst (x:xs) = (x, True) : map (,False) xs

        dSelectedGenomes <- foldDyn id (selectFirst initialDNAs) $ mconcat
            [ (\new xs -> map (,False) $ nub $ map fst xs ++ [new]) <$>
                    fmapMaybe id (tag (current dNewGenome) eAdded)
            , (\new xs -> map (,False) $ delete new (map fst xs)) <$>
                    fmapMaybe id (tag (current dNewGenome) eDelete)
            ]


        {- WebGL debugging
        el "br" blank
        elAttr "div" (mconcat
            [ ("style"  =: "width:80%; text-align: left; white-space:pre; font-family:mono")
            ]) $
          dynText (dErrors >>= sequence >>= return . foldMap (foldMap id))
        -}
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
    , "  margin:2vh;"
    , "}"
    , ".patterns canvas {"
    , "  margin:0;"
    , "  height:40vh;"
    , "  width:100%;"
    , "}"
    ]


