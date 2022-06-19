{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{- |
  This module implements the visual effect of dragging on a presentation:
   * Currently dragged things are relocated
   * Recently dragged things are moved back by animation
-}
module DragAnim
    ( MousePos
    , ObjOffset
    , offsetWithin
    , State
    , empty
    , start
    , move
    , stop
    , cleanup
    , pres
    ) where

import Data.List
import Data.Coerce

import Layout (Pos, PosAndScale)
import Presentation
import Tween

type MousePos = (Double, Double)

-- Position within an object where the user started dragging.
-- Normalized with regard to object scale, so that it is stable when
-- dragging an object that changes size
data ObjOffset = ObjOffset Double Double

offsetWithin :: MousePos -> PosAndScale -> ObjOffset
offsetWithin (x1,y1) ((x2, y2),s) = ObjOffset ((x2 - x1)/s) ((y2 - y1)/s)

addOffset :: MousePos -> Double -> ObjOffset -> PosAndScale
addOffset (x1,y1) s (ObjOffset x2 y2) = ((x1 + s * x2, y1 + s * y2), s)


data State k = State
    { currentlyDragged :: Maybe (k, Pos, ObjOffset)
    , recentlyDragged :: [(k, Pos, ObjOffset, Time)]
    }

empty :: State k
empty = State Nothing []

start :: k -> Pos -> ObjOffset -> State k -> State k
start k pos offset s =
    s { currentlyDragged = Just (k, pos, offset) }

move :: Pos -> State k -> State k
move newPos s =
    s { currentlyDragged = fmap (\(k,_,offset) -> (k, newPos, offset)) (currentlyDragged s) }

stop :: Time -> State k -> State k
stop t s = State
    { currentlyDragged = Nothing
    , recentlyDragged =
        [ (k, pos, offset, t)
        | Just (k, pos, offset) <- return (currentlyDragged s)
        ] ++ recentlyDragged s
    }

cleanup :: Time -> State k -> State k
cleanup t s = s { recentlyDragged = filter go (recentlyDragged s) }
  where
    go (_, _, _, t') = (t - t') < animationSpeed

pres ::
    Eq k =>
    State k ->
    Time ->
    (Presentation k, Animating, VideoPlaying) ->
    (Presentation k, Animating, VideoPlaying)
pres State{..} t (ps, animating, videoPlaying) =
    ( map snd (sortOn fst (map go ps))
    , coerce (||) animating (not (null recentlyDragged))
    , videoPlaying
    )
  where
    go (k, ((pos, scale), f))
        | Just (k', newPos, offset) <- currentlyDragged
        , k == k'
        = ( 2::Int
          , (k, (addOffset newPos scale offset, f))
          )
        | Just (_, p_old, offset, t') <- find (\(k', _, _, _) -> k == k') recentlyDragged
        , let r = (t-t') / animationSpeed
        , r < 1
        = ( 1
          , (k, (tween r (addOffset p_old scale offset) (pos, scale), f))
          )
        | otherwise
        = ( 0
          , (k, ((pos, scale), f))
          )

