{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Animate where

import Control.Monad.Fix
import qualified Data.Map as M
import Data.List
import Data.Maybe

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

labelDuplicatesRev :: Ord a => [(a,b)] -> [((a,Int),b)]
labelDuplicatesRev = labelDuplicates . reverse

labelDuplicates :: Ord a => [(a,b)] -> [((a,Int),b)]
labelDuplicates = snd . mapAccumL go M.empty
  where
    go m (k,v) = let n = fromMaybe 0 $ M.lookup k m in
                 (M.insert k (n+1) m, ((k,n),v))

interpolate ::
    forall m t a b.
    (MonadHold t m, MonadFix m, Reflex t, Ord b) =>
    (a -> b) ->
    Double ->
    Dynamic t Double ->
    Morpher m t a
interpolate key speed dTime dInput = do
    let eMap = toMap <$> current dTime <@> updated dInput
    dChanges <- foldDyn (pointWiseHistory speed) M.empty eMap
    return $ interp <$> dTime <*> dChanges
  where
    toMap t cur = M.fromList $
        labelDuplicatesRev [ (key k, (k, t, (p,s))) | (k,p,s) <- cur ]

    interp :: Double ->
              M.Map (b,Int) (a, ((Double,Double),Double), Double,((Double,Double),Double)) ->
              [(a, (Double,Double), Double)]
    interp t = map (interPos t) . M.elems

    interPos t (a, ((x,y),s), t',((x',y'), s'))
        | let r = (t-t') / speed, r < 1,
          let f = tween r
        = (a,(f x' x, f y' y), f s' s)
        | otherwise
        = (a,(x,y),s)

class Tweenable a where
    tween :: Double -> a -> a -> a

instance Tweenable Double where
    tween r a b
        | r < 0 = a
        | r > 1 = b
        | otherwise = (1-r) * a + r * b
instance (Tweenable a, Tweenable b) => Tweenable (a,b) where
    tween r (x,y) (x',y') = (tween r x x', tween r y y')

pointWiseHistory ::
    (Ord b, Eq c, Tweenable c) =>
    Double ->
    M.Map b (a, Double, c) ->
    M.Map b (a, c, Double, c) ->
    M.Map b (a, c, Double, c)
pointWiseHistory speed new old =
    M.fromList [ (k, go v (M.lookup k old))  | (k,v) <- M.toList new ]
  where
    go (a,t,p) Nothing = (a,p,t,p)
    go (a,t,p) (Just (_ ,p_cur, t', p'))
        | p == p_cur
        = (a, p_cur, t', p')    -- no change
        | let r = (t - t') / speed, r < 1
        = (a, p, t, tween r p' p_cur) -- a change during motion
        | otherwise
        = (a, p, t, p_cur)         -- shift


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
