{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    forall m t a b.
    (MonadHold t m, MonadFix m, Reflex t, Ord b) =>
    (a -> b) ->
    Double ->
    Dynamic t Double ->
    Morpher m t a
interpolate key speed dTime dInput = do
    let eMap = toMap <$> current dTime <@> updated dInput
    dChanges <- foldDyn keepChanges M.empty eMap
    return $ interp <$> dTime <*> dChanges
  where
    toMap t cur = M.fromList [ (key k, (k, t, (p,s))) | (k,p,s) <- cur ]

    keepChanges :: M.Map b (a, Double, ((Double,Double),Double))
                -> M.Map b (a, ((Double,Double),Double), Double,((Double,Double),Double))
                -> M.Map b (a, ((Double,Double),Double), Double,((Double,Double),Double))
    keepChanges new old =
        M.fromList [ (k, go v (M.lookup k old))  | (k,v) <- M.toList new ]
      where
        go (a,t,p) Nothing = (a,p,t,p)
        go (a,t,p) (Just v@(_ ,p_cur, t', p'))
            | p == p_cur = (a, p_cur, t', p') -- no change
            | t - t' < speed = (a, p, t', p') -- in motion
            | otherwise  = (a, p, t, (p_new,s_new)) -- shift
               where
                 (_,p_new,s_new) = interPos t v

    interp :: Double ->
              M.Map b (a, ((Double,Double),Double), Double,((Double,Double),Double)) ->
              [(a, (Double,Double), Double)]
    interp t = map (interPos t) . M.elems

    interPos t (a,((x,y),s),t',((x',y'),s'))
        | let r = (t-t') / speed, r < 1,
          let f = tween r
        = (a,(f x' x, f y' y), f s' s)
        | otherwise
        = (a,(x,y),s)

    tween r a b = (1-r) * a + r * b


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
