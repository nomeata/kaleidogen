{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module takes raw pointer event and turns them into semantic clicks and
drags.
-}
module Drag where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.IORef
import Data.Foldable
import Data.List
import Presentation (Presentation)
import qualified Presentation


type Time = Double
type MouseDelta = (Double, Double)
type MousePos = (Double, Double)

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

mkDragHandler ::
    forall k m.
    Eq k =>
    MonadIO m =>
    (Time -> m (Presentation k, Bool)) ->
    m ( Time -> RawEvent -> m [ClickEvent k]
      , Time -> m (Presentation k, Bool)
      )
mkDragHandler getPres = do
    dragState <- liftIO $ newIORef Nothing
    lastIntersection <- liftIO $ newIORef Nothing

    let getModifiedPres t = do
        (p, continue) <- getPres t
        liftIO (readIORef dragState) >>= \case
            Just (k, _, pos, True) -> do
                let go (k',(_pos,scale)) | k == k' = (k,(pos,scale))
                    go x = x
                return (sortOn (\(k',_) -> k' == k) (map go p), continue)
            _ -> return (p, continue)

    let posToKey t pos = do
        (p,_) <- getModifiedPres t
        return $ Presentation.locateClick p pos

    let intersectToKey t k = do
        (p,_) <- getModifiedPres t
        return $ Presentation.locateIntersection p k


    let finishDrag = do
            ds <- liftIO $ readIORef dragState
            liftIO $ writeIORef dragState Nothing
            liftIO $ writeIORef lastIntersection Nothing
            return ds

        handleEvent t re = execWriterT $ case re of
            MouseDown pos -> lift (posToKey t pos) >>= \case
                Just k -> do
                    liftIO $ writeIORef dragState (Just (k, t, pos, False))
                    liftIO $ writeIORef lastIntersection Nothing
                    return ()
                Nothing -> return ()
            Move pos ->
                liftIO (readIORef dragState) >>= \case
                    Just (k, t0, pos0, dragging)
                      | let delta = pos0 `sub` pos
                      , let far_enough = abs (fst delta) + abs (snd delta) > 5
                      , let long_enough = t - t0 > 100 -- in ms
                      , dragging || (far_enough && long_enough)
                      -> do
                        unless dragging $ tell [BeginDrag k]
                        liftIO $ writeIORef dragState (Just (k, t, pos, True))
                        -- tell [DragDelta delta]

                        mi_old <- liftIO $ readIORef lastIntersection
                        mi <- lift $ intersectToKey t k
                        when (mi /= mi_old) $ do
                            liftIO $ writeIORef lastIntersection mi
                            for_ mi_old $ \k' -> tell [DragOff k']
                            for_ mi $ \k' -> tell [DragOn k']
                    _ -> return ()
            MouseUp -> finishDrag >>= \case
                Just (_, _, _, True)  -> tell [EndDrag]
                Just (k, _, _, False) -> tell [Click k]
                Nothing -> return ()
            MouseOut -> finishDrag >>= \case
                Just (_, _, _, True)  -> tell [EndDrag]
                _ -> return ()


    return (handleEvent, getModifiedPres)

sub :: (Double, Double) -> (Double, Double) -> (Double, Double)
(x1,y1) `sub` (x2, y2) = (x2 - x1, y2 - y1)

