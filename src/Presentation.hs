{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NamedFieldPuns #-}
-- This module interprets a sequence of Logic.Cmd events
module Presentation
    ( Time
    , Animating(..)
    , animationSpeed
    , videoSpeed
    , Presentation
    , LayoutFun
    , initRef
    , handleCmdsRef
    , presentAtRef
    , locateClick
    , locateIntersection
    )
where

import qualified Data.Map as M
import Data.IORef
import Data.List
import Data.Bifunctor

import Layout (Pos, PosAndScale)
import PresentationCmds (Cmds, Cmd, Cmd'(..))
import Tween

type Time = Double

newtype Animating = Animating Bool

animationSpeed :: Time
animationSpeed = 200

videoSpeed :: Time
videoSpeed = 10000

data AndThen = ThenKeep | ThenDelete
data Position
    = Stable PosAndScale
    | Dynamic PosAndScale Time
    | MovingFromTo PosAndScale Time PosAndScale AndThen
data State k = State
    { pos :: M.Map k Position
    , zindex ::  M.Map k Int
    , zctr :: !Int
    }

initialState :: State k
initialState = State M.empty M.empty 0

type LayoutFun a = a -> PosAndScale

currentPos :: Ord k => Time -> k -> State k -> Maybe PosAndScale
currentPos t k s = case M.lookup k (pos s) of
    Just p -> fst <$> interpretPos t p
    Nothing -> Nothing

interpretPos :: Time -> Position -> Maybe (PosAndScale, Double)
interpretPos _ (Stable p) = Just (p, 1)
interpretPos t (Dynamic p t')
    | let r = (t-t') / videoSpeed, r < 1
    = Just (p, r)
    | otherwise
    = Just (p, 1)
interpretPos t (MovingFromTo p_old t' p_new andthen)
    | let r = (t-t') / animationSpeed
    , r < 1
    = Just (tween r p_old p_new, 1)
    | ThenDelete <- andthen
    = Nothing
    | otherwise
    = Just (p_new, 1)

handleCmd :: Ord k => Time -> LayoutFun a -> State k -> Cmd k a -> State k
handleCmd t l s@State{pos, zindex, zctr} (k, c) =
    State pos' zindex' zctr'
  where
    zctr' = zctr + 1
    pos' = case c of
        SummonAt ap -> M.insert k (Stable (l ap)) pos
        MoveTo ap
            | Just p' <- currentPos t k s
            -> M.insert k (MovingFromTo p' t (l ap) ThenKeep) pos
            | otherwise
            -> M.insert k (Stable (l ap)) pos
        FadeOut ap
            | Just p' <- currentPos t k s
            -> M.insert k (MovingFromTo p' t (l ap) ThenDelete) pos
            | otherwise
            -> M.insert k (Stable (l ap)) pos
        Animate
            | Just p' <- currentPos t k s
            -> M.insert k (Dynamic p' t) pos
            | otherwise
            -> pos
        Remove -> M.delete k pos
    zindex' = case c of
        Remove  -> M.delete k zindex
        _       -> M.insert k zctr' zindex

{-
freeze :: Time -> State -> State
freeze t = M.mapMaybe go
  where
    go p | Just pas <- interpretPos t p
         = Just (Stable pas)
         | otherwise
         = Nothing
-}

-- A presentation

type Presentation k = [(k, (PosAndScale, Double))]

presentAt :: Ord k => Time -> State k -> Presentation k
presentAt t State{pos, zindex} =
    sortOn ((zindex M.!) . fst). M.toList . M.mapMaybeWithKey go $ pos
  where
    go _ = interpretPos t

anyMoving :: Time -> State k -> Animating
anyMoving t = Animating . any go . pos
  where
    go (Stable _) = False
    go (Dynamic _ t') = t - t' < videoSpeed
    go (MovingFromTo _ t' _ _) = t - t' < animationSpeed

isIn :: (Double, Double) -> PosAndScale -> Bool
(x,y) `isIn` ((x',y'),s) = (x - x')**2 + (y - y')**2 <= s**2

locateClick :: Presentation k -> (Double, Double) -> Maybe (k, Pos)
locateClick p (x,y) = second (fst . fst) <$> find (((x,y) `isIn`) . fst . snd) p

locateIntersection :: Eq k => Presentation k -> k -> Maybe k
locateIntersection p k =
    fst <$> find (((x,y) `isIn`) . fst . snd) (filter ((/=k) . fst) p)
  where Just (((x,y),_),_) = lookup k p

-- The mutable layer
type Ref k = IORef (State k)

initRef :: IO (Ref k)
initRef = newIORef initialState

handleCmdsRef :: Ord k => Time -> LayoutFun a -> Cmds k a -> Ref k -> IO ()
handleCmdsRef t l cs r = modifyIORef r (\s -> foldl (handleCmd t l) s cs)

presentAtRef :: Ord k => Time -> Ref k -> IO (Presentation k, Double, Animating)
presentAtRef t r = do
    s <- readIORef r
    let pres = presentAt t s
        radius = minimum $ 100 : [ 2*scale | (_, (((x,y),scale),_)) <- pres, min x y < 2*scale]
            -- This is a bit of a hack: Only look at spheres near the border.
            -- The proper solution would require knowing the width and height here, so I am lazy
        continue = anyMoving t s
    return (pres, radius, continue)
