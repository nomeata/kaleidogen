{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Program
  ( BackendRunner
  , Callbacks(..)
  , Backend(..)
  , renderDNA
  , mainProgram
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.IORef
import Control.Monad.IO.Class
import Data.Foldable

import Expression
import GLSL
import DNA
import qualified SelectTwo as S2
import Layout
import Mealy
import Logic
import qualified Presentation
import Drag

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
    , onMouseOut :: m ()
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

    let mealy = logicMealy seed0
    let as0 = initial mealy
    asRef <- liftIO $ newIORef as0
    size0 <- currentWindowSize
    sizeRef <- liftIO $ newIORef size0
    pRef <- liftIO Presentation.initRef
    let handleCmds cs = do
        t <- getCurrentTime
        lf <- liftIO $ layoutFun <$> readIORef sizeRef
        liftIO $ Presentation.handleCmdsRef t lf cs pRef
    handleCmds (reconstruct mealy as0)

    let handleEvent e = do
        as <- liftIO (readIORef asRef)
        let (as', cs) = handle mealy as e
        liftIO $ writeIORef asRef as'
        handleCmds cs

    let getPresentation t = liftIO (Presentation.presentAtRef t pRef)

    (dragHandler, getModPres) <- mkDragHandler getPresentation

    let handleClickEvents re = do
        t <- getCurrentTime
        dragHandler t re >>= mapM_ (handleEvent . ClickEvent)

    return $ Callbacks
        { onDraw = do
            t <- getCurrentTime
            as <- liftIO $ readIORef asRef
            setCanDelete (S2.isOneSelected (sel as))
            setCanSave (S2.isOneSelected (sel as))
            (p, continue) <- getModPres t
            let extraData (MainInstance d) = if isSelected as d then 2 else 1
                extraData (PreviewInstance _) = 0
            let toDraw = [ (entity2dna k, (extraData k,x,y,s)) | (k,((x,y),s)) <- p ]
            return (toDraw, continue)
        , onMouseDown = handleClickEvents . MouseDown
        , onMove = handleClickEvents . Move
        , onMouseUp = handleClickEvents MouseUp
        , onMouseOut = handleClickEvents MouseOut
        , onDel = handleEvent Delete
        , onSave = do
            as <- liftIO (readIORef asRef)
            for_ (selectedDNA as) $ \dna ->
                doSave (toFilename dna) $
                    reorderExtraData
                    [ ((dna,0), layoutFullCirlce (1000, 1000) ()) ]
        , onResize = \size -> do
            liftIO $ writeIORef sizeRef size
            as <- liftIO $ readIORef asRef
            handleCmds (reconstruct mealy as)
        }
