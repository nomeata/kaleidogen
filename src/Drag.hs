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
import Data.Maybe
import Presentation (Presentation)
import qualified Presentation


type Time = Double
type MousePos = (Double, Double)
type ObjOffset = (Double, Double)

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
    MonadIO m =>
    (k -> m Bool) ->
    (Time -> m (Presentation k, Double, Bool)) ->
    m ( Time -> RawEvent -> m [ClickEvent k]
      , Time -> m (Presentation k, Double, Bool)
      )
mkDragHandler canDrag getPres = do
    dragState <- liftIO $ newIORef (Nothing :: Maybe (DragState k))
    lastIntersection <- liftIO $ newIORef Nothing

    let getModifiedPres t = do
        (p, radius, continue) <- getPres t
        liftIO (readIORef dragState) >>= \case
            Just ds  -> do
                -- NB: We always move this, even if we are not actually
                -- considering this a drag for the purpose of the game logic.
                -- This makes the UI more smooth.
                let newPos = curPos ds `add` objOffset ds
                    go (k,(_pos,scale)) | k == key ds = (k,(newPos,scale))
                    go x = x
                return (sortOn (\(k,_) -> k == key ds) (map go p), radius, continue)
            _ -> return (p, radius, continue)

    let posToKey t pos = do
        (p,_,_) <- getModifiedPres t
        return $ Presentation.locateClick p pos

    let intersectToKey t k = do
        (p,_,_) <- getModifiedPres t
        return $ Presentation.locateIntersection p k

    let finishDrag = do
            ds <- liftIO $ readIORef dragState
            liftIO $ writeIORef dragState Nothing
            liftIO $ writeIORef lastIntersection Nothing
            return ds

    let cancelDrag = finishDrag >>= \case
            Just ds | dragging ds -> tell [CancelDrag]
            _ -> return ()

    let handleEvent t re = execWriterT $ case re of
            MouseDown pos -> do
                cancelDrag
                lift (posToKey t pos) >>= \case
                    Just (k, objPos) -> lift (canDrag k) >>= \case
                        True -> do
                            liftIO $ writeIORef dragState $ Just $ DragState
                                    { initialPhase = Just (pos, t)
                                    , curPos = pos
                                    , key = k
                                    , objOffset = pos `sub` objPos
                                    }
                            liftIO $ writeIORef lastIntersection Nothing
                            return ()
                        False -> tell [Click k]
                    Nothing -> return ()
            Move pos -> do
                liftIO $ modifyIORef dragState $ fmap $ \ ds -> ds { curPos = pos }

                liftIO (readIORef dragState) >>= \case
                    Just ds | canStartDragging ds pos t -> do
                        tell [BeginDrag (key ds)]
                        liftIO $ writeIORef dragState $ Just $ ds { initialPhase = Nothing }
                    _ -> return ()

                liftIO (readIORef dragState) >>= \case
                    Just ds | dragging ds -> do
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
            MouseOut -> cancelDrag


    return (handleEvent, getModifiedPres)

sub :: (Double, Double) -> (Double, Double) -> (Double, Double)
(x1,y1) `sub` (x2, y2) = (x2 - x1, y2 - y1)


add :: (Double, Double) -> (Double, Double) -> (Double, Double)
(x1,y1) `add` (x2, y2) = (x2 + x1, y2 + y1)
