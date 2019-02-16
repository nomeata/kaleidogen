{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NamedFieldPuns #-}
-- This module interprets a sequence of Logic.Cmd events
module Presentation
    ( LayoutFun
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

import DNA (DNA)
import Logic hiding (sel)
import Layout (PosAndScale, translate)
import Tween

type Time = Double

data AndThen = ThenKeep | ThenDelete
data Position
    = Stable PosAndScale
    | MovingFromTo PosAndScale Time PosAndScale AndThen
data State = State
    { pos :: M.Map CmdKey Position
    , zindex ::  M.Map CmdKey Int
    , zctr :: !Int
    }

initialState :: State
initialState = State M.empty M.empty 0

type LayoutFun = AbstractPos -> PosAndScale

currentPos :: Time -> CmdKey -> State -> Maybe PosAndScale
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

handleCmd :: Time -> LayoutFun -> State -> Cmd -> State
handleCmd t l s@(State{pos, zindex, zctr}) c =
    State pos' zindex' zctr'
  where
    zctr' = zctr + 1
    pos' = case c of
        SummonAt k ap -> M.insert k (Stable (l ap)) pos
        MoveTo k ap
            | Just p' <- currentPos t k s
            -> M.insert k (MovingFromTo p' t (l ap) ThenKeep) pos
            | otherwise
            -> M.insert k (Stable (l ap)) pos
        ShiftPos k d
            | Just p' <- currentPos t k s
            -> M.insert k (Stable (translate d p')) pos
            | otherwise
            -> pos
        FadeOut k ap
            | Just p' <- currentPos t k s
            -> M.insert k (MovingFromTo p' t (l ap) ThenDelete) pos
            | otherwise
            -> M.insert k (Stable (l ap)) pos
        Remove k -> M.delete k pos
    zindex' = case c of
        SummonAt k _ -> M.insert k zctr' zindex
        MoveTo k _   -> M.insert k zctr' zindex
        ShiftPos k _ -> M.insert k zctr' zindex
        FadeOut k _  -> M.insert k zctr' zindex
        Remove k     -> M.delete k zindex

{-
freeze :: Time -> State -> State
freeze t = M.mapMaybe go
  where
    go p | Just pas <- interpretPos t p
         = Just (Stable pas)
         | otherwise
         = Nothing
-}

-- A presentation, including the “extra data”
type Presentation = [(CmdKey, (Double, PosAndScale))]

presentAt :: Time -> (DNA -> Bool) -> State -> Presentation
presentAt t sel State{pos, zindex} =
    sortOn ((zindex M.!) . fst). M.toList . M.mapMaybeWithKey go $ pos
  where
    go k p | Just pas <- interpretPos t p
           = Just (extraData k, pas)
           | otherwise
           = Nothing
    extraData (MainInstance d) = if sel d then 2 else 1
    extraData (PreviewInstance _) = 0

anyMoving :: Time -> State -> Bool
anyMoving t = any go . pos
  where
    go (Stable _) = False
    go (MovingFromTo _ t' _ _) = (t - t') < animationSpeed

isIn :: (Double, Double) -> PosAndScale -> Bool
(x,y) `isIn` ((x',y'),s) = (x - x')**2 + (y - y')**2 <= s**2

locateClick :: Presentation -> (Double, Double) -> Maybe CmdKey
locateClick p (x,y) =
    fst <$> find (((x,y) `isIn`) . snd . snd) p

locateIntersection :: Presentation -> CmdKey -> Maybe CmdKey
locateIntersection p k =
    fst <$> find (((x,y) `isIn`) . snd . snd) (filter ((/=k) . fst) p)
  where Just (_, ((x,y),_)) = lookup k p

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
