{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
    { drawShaderCircles :: [(a,(Double,Double,Double,Double))] -> m ()
    , animate :: (Double -> m Bool) -> m (m ())
    , setCanDelete :: Bool -> m ()
    , setCanSave :: Bool -> m ()
    , currentWindowSize :: m (Double,Double)
    , getCurrentTime :: m Double
    , onClick :: ((Double,Double) -> m ()) -> m ()
    , onDel :: m () -> m ()
    , onSave :: m () -> m ()
    , onResize :: ((Double,Double) -> m ()) -> m ()
    , doSave :: Text -> [(a,(Double,Double,Double,Double))] -> m ()
    }
type BackendRunner m = forall a.
    Ord a =>
    (a -> Text) ->
    (Backend m a -> m ()) ->
    IO ()


renderDNA :: DNA -> Text
renderDNA = toFragmentShader . dna2rna

mainProgram :: MonadIO m => Backend m DNA -> m ()
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

    let draw t = do
        as <- liftIO $ readIORef asRef
        (p, continue) <- liftIO (Presentation.presentAtRef t (isSelected as) pRef)
        drawShaderCircles [ (key2dna k, (e,x,y,s)) | (k,(e,((x,y),s))) <- M.toList p ]
        return continue
    drawAnimated <- animate draw

    let render = liftIO (readIORef asRef) >>= \AppState{..} -> do
        setCanDelete (S2.isOneSelected sel)
        setCanSave (S2.isOneSelected sel)
        drawAnimated

    let handeEvent e = do
        as <- liftIO (readIORef asRef)
        let (as', cs) = handle as e
        liftIO $ writeIORef asRef as'
        handleCmds cs
        render

    onClick $ \pos -> do
        as@AppState{..} <- liftIO (readIORef asRef)
        t <- getCurrentTime
        (p, _continue) <- liftIO (Presentation.presentAtRef t (isSelected as) pRef)
        case Presentation.locateClick p pos of
            Just k -> handeEvent (Click k)
            Nothing -> return ()
    onDel $ handeEvent Delete
    onSave $ do
        as <- liftIO (readIORef asRef)
        for_ (selectedDNA as) $ \dna -> do
            doSave (toFilename dna) $
                reorderExtraData
                [ ((dna,0), layoutFullCirlce (1000, 1000) ()) ]

    onResize $ \size -> do
        liftIO $ writeIORef sizeRef size
        as <- liftIO $ readIORef asRef
        handleCmds (initialCommands as)
        render
    return ()
