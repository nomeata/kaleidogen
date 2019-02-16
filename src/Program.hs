{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Program where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid
import Data.IORef
import Control.Monad.IO.Class
import Data.Foldable

import Expression
import GLSL
import DNA
import qualified SelectTwo as S2
import Layout
import Logic
import qualified Presentation

reorderExtraData :: [((DNA, a), ((b,c),d))] -> [(DNA, (a, b, c, d))]
reorderExtraData = map $ \((d,b),((x,y),s)) -> (d, (b, x, y, s))

toFilename :: DNA -> T.Text
toFilename dna = "kaleidogen-" <> dna2hex dna <> ".png"

layoutFun :: (Double, Double) -> AbstractPos -> PosAndScale
layoutFun size MainPos
    = topHalf layoutFullCirlce size ()
layoutFun size (SmallPos c n)
    = bottomHalf (layoutGrid False c) size n
layoutFun size (DeletedPos c n)
    = bottomHalf (layoutGrid True c) size n

getLayoutFun :: IORef (Double, Double) -> IO Presentation.LayoutFun
getLayoutFun r = do
    size <- readIORef r
    return (layoutFun size)

data Backend m a = Backend
    { setCanDelete :: Bool -> m ()
    , setCanSave :: Bool -> m ()
    , currentWindowSize :: m (Double,Double)
    , getCurrentTime :: m Double
    , doSave :: Text -> [(a,(Double,Double,Double,Double))] -> m ()

    }
data Callbacks m a = Callbacks
    { onDraw :: m ([(a,(Double,Double,Double,Double))], Bool)
    , onMouseDown :: (Double,Double) -> m ()
    , onMove :: (Double,Double) -> m ()
    , onMouseUp :: m ()
    , onDel :: m ()
    , onSave :: m ()
    , onResize :: (Double,Double) -> m ()
    }
type BackendRunner m = forall a.
    Ord a =>
    (a -> Text) ->
    (Backend m a -> m (Callbacks m a)) ->
    m ()


renderDNA :: DNA -> Text
renderDNA = toFragmentShader . dna2rna

mainProgram :: MonadIO m => Backend m DNA -> m (Callbacks m DNA)
mainProgram Backend {..} = do
    -- Set up global state
    seed0 <- liftIO getRandom
    let as0 = initialAppState seed0
    asRef <- liftIO $ newIORef as0
    size0 <- currentWindowSize
    sizeRef <- liftIO $ newIORef size0
    pRef <- liftIO Presentation.initRef
    let handleCmds cs = do
        t <- getCurrentTime
        lf <- liftIO $ getLayoutFun sizeRef
        liftIO $ Presentation.handleCmdsRef t lf cs pRef
    handleCmds (initialCommands as0)

    -- State to detect clicks vs. drags
    dragState <- liftIO $ newIORef Nothing

    let handeEvent e = do
        as <- liftIO (readIORef asRef)
        let (as', cs) = handle as e
        liftIO $ writeIORef asRef as'
        handleCmds cs

    let clickToCmdKey pos = do
        as@AppState{..} <- liftIO (readIORef asRef)
        t <- getCurrentTime
        (p, _continue) <- liftIO (Presentation.presentAtRef t (isSelected as) pRef)
        return $ Presentation.locateClick p pos

    return $ Callbacks
        { onDraw = do
            t <- getCurrentTime
            as <- liftIO $ readIORef asRef
            setCanDelete (S2.isOneSelected (sel as))
            setCanSave (S2.isOneSelected (sel as))
            (p, continue) <- liftIO (Presentation.presentAtRef t (isSelected as) pRef)
            let toDraw = [ (key2dna k, (e,x,y,s)) | (k,(e,((x,y),s))) <- M.toList p ]
            return (toDraw, continue)
        , onMouseDown = \pos ->
            clickToCmdKey pos >>= \case
                Just k -> do
                    liftIO $ writeIORef dragState (Just (pos, False))
                    handeEvent (BeginDrag k)
                Nothing -> return ()
        , onMove = \pos ->
            liftIO (readIORef dragState) >>= \case
                Just (pos0, _) -> do
                    liftIO $ writeIORef dragState (Just (pos,True))
                    handeEvent (DragDelta (pos0 `sub` pos))
                Nothing -> return ()
        , onMouseUp = do
            liftIO (readIORef dragState) >>= \case
                Just (_, True)  -> handeEvent EndDrag
                Just (_, False) -> handeEvent EndClick
                Nothing -> return ()
            liftIO $ writeIORef dragState Nothing

        , onDel = handeEvent Delete
        , onSave = do
            as <- liftIO (readIORef asRef)
            for_ (selectedDNA as) $ \dna ->
                doSave (toFilename dna) $
                    reorderExtraData
                    [ ((dna,0), layoutFullCirlce (1000, 1000) ()) ]
        , onResize = \size -> do
            liftIO $ writeIORef sizeRef size
            as <- liftIO $ readIORef asRef
            handleCmds (initialCommands as)
        }

sub :: (Double, Double) -> (Double, Double) -> (Double, Double)
(x1,y1) `sub` (x2, y2) = (x2 - x1, y2 - y1)
