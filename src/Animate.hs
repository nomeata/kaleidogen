{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Animate where

import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.IORef
import Data.Function

import GHCJS.DOM
import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM.Performance
import GHCJS.DOM.GlobalPerformance

type LaidOut a = [(a, (Double, Double), Double)]

type Morpher a =
    (LaidOut a -> JSM ()) ->
    JSM (LaidOut a -> JSM ())

labelDuplicatesRev :: Ord a => [(a,b)] -> [((a,Int),b)]
labelDuplicatesRev = labelDuplicates . reverse

labelDuplicates :: Ord a => [(a,b)] -> [((a,Int),b)]
labelDuplicates = snd . mapAccumL go M.empty
  where
    go m (k,v) = let n = fromMaybe 0 $ M.lookup k m in
                 (M.insert k (n+1) m, ((k,n),v))

interpolate ::
    forall a b.
    Ord b =>
    (a -> b) ->
    Double ->
    Morpher a
interpolate key speed draw = do
    Just win <- currentWindow
    perf <- getPerformance win
    s <- liftIO $ newIORef M.empty

    let drawCurrent t = do
        changes <- liftIO $ readIORef s
        draw (interp t changes)

    fix (\go -> () <$ inAnimationFrame' (\t -> drawCurrent t >> go))

    return $ \input -> do
        t <- now perf
        let dm = toMap input
        liftIO $ modifyIORef s (pointWiseHistory speed t dm)
        drawCurrent t
  where
    toMap cur = M.fromList $
        labelDuplicatesRev [ (key k, (k, (p,s))) | (k,p,s) <- cur ]

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
    Double ->
    M.Map b (a, c) ->
    M.Map b (a, c, Double, c) ->
    M.Map b (a, c, Double, c)
pointWiseHistory speed t new old =
    M.fromList [ (k, go v (M.lookup k old))  | (k,v) <- M.toList new ]
  where
    go (a,p) Nothing = (a,p,t,p)
    go (a,p) (Just (_ ,p_cur, t', p'))
        | p == p_cur
        = (a, p_cur, t', p')    -- no change
        | let r = (t - t') / speed, r < 1
        = (a, p, t, tween r p' p_cur) -- a change during motion
        | otherwise
        = (a, p, t, p_cur)         -- shift


compareLaidOut :: Eq a => LaidOut a -> LaidOut a -> LaidOut a
compareLaidOut [] news =  news
compareLaidOut _ [] = []
compareLaidOut ((a1, (_x1, _y1), _s1) : olds) ((a2,(x2,y2),s2):news)
    | a1 == a2 = (a2, (x2, y2), s2)  : compareLaidOut olds news
    | otherwise = (a2, (x2,y2), 2*s2) : compareLaidOut olds news
