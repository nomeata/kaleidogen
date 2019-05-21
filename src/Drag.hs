{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.IORef
import Data.Foldable
import Data.List
import Presentation (Presentation)
import qualified Presentation


type Time = Double
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

data DragState k = DragState
    { dragging :: Bool
    , key :: k
    , startTime :: Time
    , startPos :: MousePos
    }

mkDragHandler ::
    forall k m.
    Eq k =>
    MonadIO m =>
    (Time -> m (Presentation k, Bool)) ->
    m ( Time -> RawEvent -> m [ClickEvent k]
      , Time -> m (Presentation k, Bool)
      )
mkDragHandler getPres = do
    dragState <- liftIO $ newIORef (Nothing :: Maybe (DragState k))
    lastIntersection <- liftIO $ newIORef Nothing

    let getModifiedPres t = do
        (p, continue) <- getPres t
        liftIO (readIORef dragState) >>= \case
            Just ds | dragging ds -> do
                let go (k,(_pos,scale)) | k == key ds = (k,(startPos ds,scale))
                    go x = x
                return (sortOn (\(k,_) -> k == key ds) (map go p), continue)
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
                    liftIO $ writeIORef dragState $ Just $ DragState
                            { dragging = False
                            , startPos = pos
                            , startTime = t
                            , key = k
                            }
                    liftIO $ writeIORef lastIntersection Nothing
                    return ()
                Nothing -> return ()
            Move pos ->
                liftIO (readIORef dragState) >>= \case
                    Just ds
                      | let delta = startPos ds `sub` pos
                      , let far_enough = abs (fst delta) + abs (snd delta) > 5
                      , let long_enough = t - startTime ds > 100 -- in ms
                      , dragging ds || (far_enough && long_enough)
                      -> do
                        unless (dragging ds) $ tell [BeginDrag (key ds)]
                        liftIO $ writeIORef dragState $ Just $ ds
                            { dragging = True
                            , startPos = pos
                            , startTime = t
                            }
                        -- tell [DragDelta delta]

                        mi_old <- liftIO $ readIORef lastIntersection
                        mi <- lift $ intersectToKey t (key ds)
                        when (mi /= mi_old) $ do
                            liftIO $ writeIORef lastIntersection mi
                            for_ mi_old $ \k' -> tell [DragOff k']
                            for_ mi $ \k' -> tell [DragOn k']
                    _ -> return ()
            MouseUp -> finishDrag >>= \case
                Just ds | dragging ds -> tell [EndDrag]
                        | otherwise   -> tell [Click (key ds)]
                Nothing -> return ()
            MouseOut -> finishDrag >>= \case
                Just ds | dragging ds -> tell [EndDrag]
                _ -> return ()


    return (handleEvent, getModifiedPres)

sub :: (Double, Double) -> (Double, Double) -> (Double, Double)
(x1,y1) `sub` (x2, y2) = (x2 - x1, y2 - y1)

