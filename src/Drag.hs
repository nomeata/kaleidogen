{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
This module takes raw pointer event and turns them into semantic clicks and
drags.
-}
module Drag
    ( ClickEvent(..)
    , RawEvent(..)
    , mkDragHandler
    ) where


import Control.Monad
import Control.Monad.Writer
import Control.Monad.Ref
import Data.Foldable
import Data.Maybe
import Presentation (Presentation, Time, Animating, VideoPlaying)
import qualified Presentation
import DragAnim (MousePos, ObjOffset, offsetWithin)
import qualified DragAnim

data RawEvent
    = MouseDown MousePos
    | Move MousePos
    | MouseUp
    | MouseOut

data ClickEvent k
    = Click k
    | BeginDrag k
    | DragOn k
    | DragOff k
    | EndDrag
    | CancelDrag
    | OtherClick
  deriving Show

data DragState k = DragState
    { initialPhase :: Maybe (MousePos, Time)
    , key :: k
    , curPos :: MousePos
    , objOffset :: ObjOffset
    }

canStartDragging :: DragState k -> MousePos -> Time -> Bool
canStartDragging ds pos t
  | Just (startPos, startTime) <- initialPhase ds
  , let delta = startPos `sub` pos
  , let far_enough = abs (fst delta) + abs (snd delta) > 5
  , let long_enough = t - startTime > 100 -- in ms
  = far_enough && long_enough
  | otherwise
  = False

dragging :: DragState k -> Bool
dragging = isNothing . initialPhase

mkDragHandler ::
    forall k m.
    Eq k =>
    MonadRef m =>
    (k -> m Bool) ->
    (Time -> m (Presentation k, Animating, VideoPlaying)) ->
    m ( Time -> RawEvent -> m [ClickEvent k]
      , Time -> m (Presentation k, Animating, VideoPlaying)
      , m ()
      )
mkDragHandler canDrag getPres = do
    dragState <- newRef (Nothing :: Maybe (DragState k))
    dragAnimState <- newRef DragAnim.empty
    lastIntersection <- newRef Nothing

    let getModifiedPres t = do
          modifyRef dragAnimState (DragAnim.cleanup t)
          DragAnim.pres <$> readRef dragAnimState
                        <*> pure t
                        <*> getPres t

    let posToKey t pos = do
          (p,_,_) <- getModifiedPres t
          return $ Presentation.locateClick p pos

    let intersectToKey t k = do
          (p,_,_) <- getModifiedPres t
          return $ Presentation.locateIntersection p k

    let finishDrag t = do
          ds <- readRef dragState
          writeRef dragState Nothing
          writeRef lastIntersection Nothing
          modifyRef dragAnimState (DragAnim.stop t)
          return ds

    let cancelDrag t = finishDrag t >>= \case
            Just ds | dragging ds -> tell [CancelDrag]
            _ -> return ()

    let handleEvent t re = execWriterT $ case re of
            MouseDown pos -> do
                cancelDrag t
                lift (posToKey t pos) >>= \case
                    Just (k, objPos) -> lift (canDrag k) >>= \case
                        True -> do
                            let offset = offsetWithin pos objPos
                            writeRef dragState $ Just $ DragState
                                { initialPhase = Just (pos, t)
                                , curPos = pos
                                , key = k
                                , objOffset = offset
                                }
                            writeRef lastIntersection Nothing
                            modifyRef dragAnimState (DragAnim.start k pos offset)
                                -- NB: We always display moving the drag, even if the drag
                                -- is still small enough that it might just be a click, and
                                -- before we report it to the game logic.
                                -- This makes the UI more smooth.
                            return ()
                        False -> tell [Click k]
                    Nothing -> tell [OtherClick]
            Move pos -> do
                modifyRef dragState $ fmap $ \ ds -> ds { curPos = pos }
                modifyRef dragAnimState (DragAnim.move pos)

                readRef dragState >>= \case
                    Just ds | canStartDragging ds pos t -> do
                        tell [BeginDrag (key ds)]
                        writeRef dragState $ Just $ ds { initialPhase = Nothing }
                    _ -> return ()

                readRef dragState >>= \case
                    Just ds | dragging ds -> do
                        mi_old <- readRef lastIntersection
                        mi <- lift $ intersectToKey t (key ds)
                        when (mi /= mi_old) $ do
                            writeRef lastIntersection mi
                            for_ mi_old $ \k' -> tell [DragOff k']
                            for_ mi $ \k' -> tell [DragOn k']
                    _ -> return ()
            MouseUp -> finishDrag t >>= \case
                Just ds | dragging ds -> tell [EndDrag]
                        | otherwise   -> tell [Click (key ds)]
                Nothing -> return ()
            MouseOut -> cancelDrag t


    let reset = do
            writeRef dragState (Nothing :: Maybe (DragState k))
            writeRef dragAnimState DragAnim.empty
            writeRef lastIntersection Nothing

    return (handleEvent, getModifiedPres, reset)

sub :: MousePos -> MousePos -> (Double, Double)
(x1,y1) `sub` (x2, y2) = (x2 - x1, y2 - y1)
