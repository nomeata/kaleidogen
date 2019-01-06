{-# LANGUAGE PatternGuards #-}
-- This module interprets a sequence of Logic.Cmd events
module Presentation where

import qualified Data.Map as M
import Data.IORef
import Data.List

import DNA (DNA)
import Logic hiding (sel)
import Layout (PosAndScale)
import Tween

type Time = Double

data AndThen = ThenKeep | ThenDelete
data Position
    = Stable PosAndScale
    | MovingFromTo PosAndScale Time PosAndScale AndThen
type State = M.Map CmdKey Position

initialState :: State
initialState = M.empty

type LayoutFun = AbstractPos -> PosAndScale

currentPos :: Time -> CmdKey -> State -> Maybe PosAndScale
currentPos t k s = case M.lookup k s of
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

handleCmd :: Time -> LayoutFun -> State -> Cmd -> State
handleCmd t l s c = case c of
    SummonAt k ap -> M.insert k (Stable (l ap)) s
    MoveTo k ap
        | Just p' <- currentPos t k s
        -> M.insert k (MovingFromTo p' t (l ap) ThenKeep) s
        | otherwise
        -> M.insert k (Stable (l ap)) s
    FadeOut k ap
        | Just p' <- currentPos t k s
        -> M.insert k (MovingFromTo p' t (l ap) ThenDelete) s
        | otherwise
        -> M.insert k (Stable (l ap)) s
    Remove k -> M.delete k s

freeze :: Time -> State -> State
freeze t = M.mapMaybe go
  where
    go p | Just pas <- interpretPos t p
         = Just (Stable pas)
         | otherwise
         = Nothing

-- A presentation, including the “extra data”
type Presentation = M.Map CmdKey (Double, PosAndScale)

presentAt :: Time -> (DNA -> Bool) -> State -> Presentation
presentAt t sel = M.mapMaybeWithKey go
  where
    go k p | Just pas <- interpretPos t p
           = Just (extraData k, pas)
           | otherwise
           = Nothing
    extraData (MainInstance d) = if sel d then 2 else 1
    extraData (PreviewInstance _) = 0

anyMoving :: Time -> State -> Bool
anyMoving t = any go
  where
    go (Stable _) = False
    go (MovingFromTo _ t' _ _) = (t - t') < animationSpeed

locateClick :: Presentation -> (Double, Double) -> Maybe CmdKey
locateClick p (x,y) = fst <$> find go (M.toList p)
  where
    go (_, (_, ((x',y'),s))) = (x - x')**2 + (y - y')**2 <= s**2

-- The mutable layer
type Ref = IORef State

initRef :: IO Ref
initRef = newIORef initialState

handleCmdsRef :: Time -> LayoutFun -> Cmds -> Ref -> IO ()
handleCmdsRef t l cs r = modifyIORef r (\s -> foldl (handleCmd t l) s cs)

presentAtRef :: Time -> (DNA -> Bool) -> Ref -> IO (Presentation, Bool)
presentAtRef t sel r = do
    s <- readIORef r
    return (presentAt t sel s, anyMoving t s)
