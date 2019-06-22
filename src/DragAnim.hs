{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{- |
  This module implements the visual effect of draggin on a presentation:
   * Currently dragged things are relocated
   * Recently dragged things are moved back by animation
-}
module DragAnim
    ( State
    , empty
    , start
    , move
    , stop
    , cleanup
    , pres
    ) where

import Data.List
import Data.Coerce

import Layout (Pos)
import Presentation
import Tween

data State k = State
    { currentlyDragged :: Maybe (k, Pos, Pos)
        -- the second Pos is the offset, makes the client code simpler
    , recentlyDragged :: [(k, Pos, Time)]
    }

empty :: State k
empty = State Nothing []

start :: k -> Pos -> Pos -> State k -> State k
start k pos offset s =
    s { currentlyDragged = Just (k, pos, offset) }

move :: Pos -> State k -> State k
move newPos s =
    s { currentlyDragged = fmap (\(k,_,offset) -> (k, newPos, offset)) (currentlyDragged s) }

stop :: Time -> State k -> State k
stop t s = State
    { currentlyDragged = Nothing
    , recentlyDragged =
        [ (k, pos `add` offset, t)
        | Just (k, pos, offset) <- return (currentlyDragged s)
        ] ++ recentlyDragged s
    }

cleanup :: Time -> State k -> State k
cleanup t s = s { recentlyDragged = filter go (recentlyDragged s) }
  where
    go (_, _, t') = (t - t') < animationSpeed

pres ::
    Eq k =>
    State k ->
    Time ->
    (Presentation k, Double, Animating) ->
    (Presentation k, Double, Animating)
pres State{..} t (ps, radius, animating) =
    ( map snd (sortOn fst (map go ps))
    , radius
    , coerce (||) animating (not (null recentlyDragged))
    )
  where
    go (k, (pos, scale))
        | Just (k', newPos, offset) <- currentlyDragged
        , k == k'
        = ( 2::Int
          , (k, (newPos `add` offset, scale))
          )
        | Just (_, p_old, t') <- find (\(k', _, _) -> k == k') recentlyDragged
        , let r = (t-t') / animationSpeed
        , r < 1
        = ( 1
          , (k, (tween r p_old pos, scale))
          )
        | otherwise
        = ( 0
          , (k, (pos, scale))
          )

add :: (Double, Double) -> (Double, Double) -> (Double, Double)
(x1,y1) `add` (x2, y2) = (x2 + x1, y2 + y1)

