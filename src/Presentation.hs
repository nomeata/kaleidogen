{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NamedFieldPuns #-}
-- This module interprets a sequence of Logic.Cmd events
module Presentation
    ( Presentation
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

animationSpeed :: Time
animationSpeed = 200

data AndThen = ThenKeep | ThenDelete
data Position
    = Stable PosAndScale
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
    Just p -> interpretPos t p
    Nothing -> Nothing

interpretPos :: Time -> Position -> Maybe PosAndScale
interpretPos _ (Stable p) = Just p
interpretPos t (MovingFromTo p_old t' p_new andthen)
    | let r = (t-t') / animationSpeed
    , r < 1
    = Just (tween r p_old p_new)
    | ThenDelete <- andthen
    = Nothing
    | otherwise
    = Just p_new

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
        Remove -> M.delete k pos
    zindex' = case c of
        SummonAt _ -> M.insert k zctr' zindex
        MoveTo _   -> M.insert k zctr' zindex
        FadeOut _  -> M.insert k zctr' zindex
        Remove     -> M.delete k zindex

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

type Presentation k = [(k, PosAndScale)]

presentAt :: Ord k => Time -> State k -> Presentation k
presentAt t State{pos, zindex} =
    sortOn ((zindex M.!) . fst). M.toList . M.mapMaybeWithKey go $ pos
  where
    go _ = interpretPos t

anyMoving :: Time -> State k -> Bool
anyMoving t = any go . pos
  where
    go (Stable _) = False
    go (MovingFromTo _ t' _ _) = (t - t') < animationSpeed

isIn :: (Double, Double) -> PosAndScale -> Bool
(x,y) `isIn` ((x',y'),s) = (x - x')**2 + (y - y')**2 <= s**2

locateClick :: Presentation k -> (Double, Double) -> Maybe (k, Pos)
locateClick p (x,y) = second fst <$> find (((x,y) `isIn`) . snd) p

locateIntersection :: Eq k => Presentation k -> k -> Maybe k
locateIntersection p k =
    fst <$> find (((x,y) `isIn`) . snd) (filter ((/=k) . fst) p)
  where Just ((x,y),_) = lookup k p

-- The mutable layer
type Ref k = IORef (State k)

initRef :: IO (Ref k)
initRef = newIORef initialState

handleCmdsRef :: Ord k => Time -> LayoutFun a -> Cmds k a -> Ref k -> IO ()
handleCmdsRef t l cs r = modifyIORef r (\s -> foldl (handleCmd t l) s cs)

presentAtRef :: Ord k => Time -> Ref k -> IO (Presentation k, Bool)
presentAtRef t r = do
    s <- readIORef r
    return (presentAt t s, anyMoving t s)
