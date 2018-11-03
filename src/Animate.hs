{-# LANGUAGE PatternGuards #-}
module Animate where

import Control.Monad.Fix
import qualified Data.Map as M

import Reflex.Dom

type LaidOut a = [(a, (Double, Double), Double)]

type Morpher m t a = Dynamic t (LaidOut a) -> m (Dynamic t (LaidOut a))

idMorpher :: Monad m => Morpher m t a
idMorpher = return

wiggle :: (Monad m, Reflex t) => Dynamic t Double -> Morpher m t a
wiggle dt das = return $ zipDynWith doWiggle dt das

doWiggle :: Double -> LaidOut a -> LaidOut a
doWiggle t as =
    [ (a, (x + 10 * sin (t/1000),y + 10 * cos (t/1000)), s)
    | (a, (x,y), s) <- as]

interpolate ::
    (MonadHold t m, MonadFix m, Reflex t, Ord b) =>
    (a -> b) ->
    Double ->
    Dynamic t Double ->
    Morpher m t a
interpolate key speed dTime dInput = do
    dPreviousPosition <- foldDyn id M.empty (go <$> current dTime <@> updated dInput)
    return $ interp <$> dTime <*> dPreviousPosition <*> dInput
  where
    toMap t cur = M.fromList [ (key k, (t,(p,s))) | (k,p,s) <- cur ]
    go t new lastChanged = M.unionWith update lastChanged (toMap t new)
    update (t',x') (t,x)
        | x == x'      = (t',x')
        | t-t' < speed = (t',x')
        | otherwise    = (t,x)

    interp t lastChanged = map $ \(k,(x,y),s) ->
        case M.lookup (key k) lastChanged of
            Just (t',((x',y'),s'))
                | let r = (t-t') / speed
                , r < 1
              -> let f a b = b in -- (1-r) * a + r * b in
                 (k,(f x' x, f y' y), 2 * f s' s)
            _ -> (k,(x,y),s)



highlightChanged :: (MonadHold t m, MonadFix m, Reflex t, Eq a) => Morpher m t a
highlightChanged dInput = do
    dPrev <- fmap fst <$> foldDyn go ([],[]) (updated dInput)
    return $ zipDynWith compareLaidOut dPrev dInput
  where
    go now (_, prev) = (prev,now)

compareLaidOut :: Eq a => LaidOut a -> LaidOut a -> LaidOut a
compareLaidOut [] news =  news
compareLaidOut _ [] = []
compareLaidOut ((a1, (_x1, _y1), _s1) : olds) ((a2,(x2,y2),s2):news)
    | a1 == a2 = (a2, (x2, y2), s2)  : compareLaidOut olds news
    | otherwise = (a2, (x2,y2), 2*s2) : compareLaidOut olds news
