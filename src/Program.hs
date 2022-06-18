{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Program
  ( ProgramRunner
  , Callbacks(..)
  , Time
  , DrawResult(..)
  , renderGraphic
  , mainProgram
  , tutorialProgram
  , Graphic
  , showFullDNA
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import Data.IORef
import Control.Monad.IO.Class
import Data.Maybe

import Shaders
import Expression
import GLSL
import DNA
import Layout
import Mealy
import Logic
import Presentation (Animating)
import qualified Presentation
import Drag
import qualified Tutorial as Tut
import Tween

reorderExtraData :: ((x, a), (((b,c),d),e)) -> (x, (a, b, c, d, e))
reorderExtraData ((d,b),(((x,y),s),e)) = (d, (b, x, y, s, e))

toFilename :: DNA -> T.Text
toFilename dna = "kaleidogen-" <> dna2hex dna <> ".png"

layoutFun :: (Double, Double) -> AbstractPos -> PosAndScale
layoutFun size MainPos
    = topHalf (padding layoutFullCirlce) size ()
layoutFun size (SmallPos c n)
    = bottomHalf (padding (layoutGrid c)) size n
layoutFun size DeletedPos
    = topHalf (padding layoutCenterDot) size ()


showFullDNA :: DNA -> (Double,Double) -> (Graphic, ExtraData)
showFullDNA dna (w,h) =
    reorderExtraData ((DNA dna,0), (layoutFullCirlce (w,h) (), 1))

type Time = Double

data DrawResult a = DrawResult
    { objects :: [(a, ExtraData)]
    , stillAnimating :: Animating
    , canDelete :: Bool
    , canSave :: Bool
    , canAnim :: Bool
    }

data Callbacks m a = Callbacks
    { onDraw      :: Time -> m (DrawResult a)
    , onMouseDown :: Time -> (Double,Double) -> m ()
    , onMove      :: Time -> (Double,Double) -> m ()
    , onMouseUp   :: Time -> m ()
    , onMouseOut  :: Time -> m ()
    , onDel       :: Time -> m ()
    , onSave      :: Time -> m (Maybe (Text, (a, ExtraData)))
    , onAnim      :: Time -> m ()
    , onResize    :: Time -> (Double,Double) -> m ()
    }

type ProgramRunner m = forall a.
    Ord a =>
    (a -> Shaders) ->
    (Time -> (Double, Double) -> m (Callbacks m a)) ->
    m ()

data Graphic = DNA DNA | Border | Mouse deriving (Eq, Ord)

renderGraphic :: Graphic -> (Text, Text)
renderGraphic (DNA d) = (circularVertexShader, toFragmentShader (dna2rna d))
renderGraphic Border = borderShaders
renderGraphic Mouse  = (circularVertexShader, mouseFragmentShader)


mainProgram :: MonadIO m => Time -> (Double, Double) -> m (Callbacks m Graphic)
mainProgram t0 size0 = do
    -- Set up global state
    seed0 <- liftIO getRandom

    let mealy = logicMealy seed0
    let as0 = initial mealy
    asRef <- liftIO $ newIORef as0
    sizeRef <- liftIO $ newIORef size0
    pRef <- liftIO Presentation.initRef
    let handleCmds t cs = do
          lf <- liftIO $ layoutFun <$> readIORef sizeRef
          liftIO $ Presentation.handleCmdsRef t lf cs pRef
    handleCmds t0 (reconstruct mealy as0)

    let handleEvent t e = do
          as <- liftIO (readIORef asRef)
          let (as', cs) = handle mealy as e
          liftIO $ writeIORef asRef as'
          handleCmds t cs

    let getPresentation t = liftIO (Presentation.presentAtRef t pRef)

    let canDragM k = do
          as <- liftIO (readIORef asRef)
          return (Logic.canDrag as k)

    (dragHandler, getModPres, _resetDrag) <- mkDragHandler canDragM getPresentation

    let handleClickEvents t re = do
          dragHandler t re >>= mapM_ (handleEvent t . ClickEvent)

    return $ Callbacks
        { onDraw = \t -> do
            as <- liftIO $ readIORef asRef
            let canDelete = isJust (sel as)
            let canSave   = isJust (sel as)
            let canAnim   = isJust (sel as)
            (p, stillAnimating) <- getModPres t
            -- Calcualting the border radius
            size <- liftIO $ readIORef sizeRef
            -- A bit of a hack to access as here
            let borderRadius = gridBorderRadius (M.size (dnas as)) size
            let extraData d
                  -- | isSelected as d = 2
                  | isInactive as d = 3
                  | otherwise       = 0
            let objects =
                    (Border, (0,0,0,borderRadius,1)) :
                    [ (DNA (entity2dna k), (extraData k,x,y,s,f)) | (k,(((x,y),s),f)) <- p ]
            return (DrawResult {..})
        , onMouseDown = \t -> handleClickEvents t . MouseDown
        , onMove = \t -> handleClickEvents t . Move
        , onMouseUp = \t -> handleClickEvents t MouseUp
        , onMouseOut = \t -> handleClickEvents t MouseOut
        , onDel = \t -> handleEvent t Delete
        , onAnim = \t -> handleEvent t Anim
        , onSave = \_ -> do
            as <- liftIO (readIORef asRef)
            pure $ (\dna -> (toFilename dna, showFullDNA dna (1000,1000))) <$> selectedDNA as
        , onResize = \t size -> do
            liftIO $ writeIORef sizeRef size
            as <- liftIO $ readIORef asRef
            handleCmds t (reconstruct mealy as)
        }

-- TODO: Find new abstractions to remove duplication with above
-- TODO: Make it all pure? Or state monad?

tutorialProgram :: MonadIO m => Time -> (Double,Double) -> m (Callbacks m Graphic)
tutorialProgram t0 size0 = do
    -- Fixed state
    let seed0  = 1

    let mealy = logicMealy seed0
    let as0 = initial mealy
    asRef <- liftIO $ newIORef as0
    sizeRef <- liftIO $ newIORef size0
    pRef <- liftIO Presentation.initRef

    let handleCmds t cs = do
          lf <- liftIO $ layoutFun <$> readIORef sizeRef
          liftIO $ Presentation.handleCmdsRef t lf cs pRef
    handleCmds t0 (reconstruct mealy as0)

    let handleEvent t e = do
          as <- liftIO (readIORef asRef)
          let (as', cs) = handle mealy as e
          liftIO $ writeIORef asRef as'
          handleCmds t cs

    let getPresentation t = liftIO (Presentation.presentAtRef t pRef)

    let canDragM k = do
          as <- liftIO (readIORef asRef)
          return (Logic.canDrag as k)

    (dragHandler, getModPres, resetDrag) <- mkDragHandler canDragM getPresentation

    let handleClickEvents t re = do
          dragHandler t re >>= mapM_ (handleEvent t . ClickEvent)

    -- Tutorial animation handling

    let getPosOf t (Tut.DNA n v) = do
          as <- liftIO $ readIORef asRef
          let d = dnas as M.! Key n
          (p, _continue) <- getModPres t
          let Just (((x,y),s),_f) = L.lookup d p
          case v of
            Tut.NE -> pure (x + s/5,y - s/5)
            Tut.W  -> pure (x - s/4,y)
        getPosOf _ Tut.Center = do
          (w,h) <- liftIO $ readIORef sizeRef
          pure (w/2, h/2)

    scriptRef <- liftIO $ newIORef Tut.tutorial
    scriptStepStartRef <- liftIO $ newIORef t0
    lastMousePosition <- getPosOf t0 Tut.Center >>= liftIO . newIORef
    currentMousePos <- liftIO $ readIORef lastMousePosition >>= newIORef
    mouseDownRef <- liftIO $ newIORef False
    let tickAnimation t = do
            s <- liftIO $ readIORef scriptRef
            (x1,y1) <- liftIO $ readIORef lastMousePosition
            t1 <- liftIO $ readIORef scriptStepStartRef
            case s of
                [] -> pure ()
                (Tut.Wait n):s'
                  | t > t1 + n -> do
                    -- Wait is done
                    liftIO $ writeIORef scriptStepStartRef (t1 + n)
                    liftIO $ writeIORef scriptRef s'
                    tickAnimation t
                  | otherwise -> pure () -- Wait is not done
                (Tut.MoveMouseTo d n):s'
                  | t > t1 + n -> do
                    -- Mouse move is done
                    (x,y) <- getPosOf (t1 + n) d
                    handleClickEvents (t1 + n) (Move (x,y))
                    liftIO $ writeIORef scriptStepStartRef (t1 + n)
                    liftIO $ writeIORef lastMousePosition (x,y)
                    liftIO $ writeIORef currentMousePos (x,y)
                    liftIO $ writeIORef scriptRef s'
                    tickAnimation t
                  | otherwise -> do
                    -- Mouse move is happening
                    (x,y) <- getPosOf (t1 + n) d
                    let mousePos = tween ((t-t1)/n) (x1,y1) (x,y)
                    handleClickEvents (t1 + n) (Move mousePos)
                    liftIO $ writeIORef currentMousePos mousePos
                Tut.MouseDown:s' -> do
                    handleClickEvents t1 (MouseDown (x1,y1))
                    liftIO $ writeIORef scriptRef s'
                    liftIO $ writeIORef mouseDownRef True
                    tickAnimation t
                Tut.MouseUp:s' -> do
                    handleClickEvents t1 MouseUp
                    liftIO $ writeIORef scriptRef s'
                    liftIO $ writeIORef mouseDownRef False
                    tickAnimation t
                Tut.StartAnim:s' -> do
                    handleEvent t1 Anim
                    liftIO $ writeIORef scriptRef s'
                    tickAnimation t



    return $ Callbacks
        { onDraw = \t -> do
            tickAnimation t

            as <- liftIO $ readIORef asRef
            let canDelete = isJust (sel as)
            let canSave = isJust (sel as)
            let canAnim = isJust (sel as)
            (p, _continue) <- getModPres t
            -- Calcualting the border radius
            size <- liftIO $ readIORef sizeRef
            -- A bit of a hack to access as here
            let borderRadius = gridBorderRadius (M.size (dnas as)) size
            let extraData d
                  -- | isSelected as d = 2
                  | isInactive as d = 3
                  | otherwise       = 0
            isMouseDown <- liftIO $ readIORef mouseDownRef
            let mouseExtraData
                  | isMouseDown = 1
                  | otherwise   = 0
            (mx,my) <- liftIO $ readIORef currentMousePos
            let objects =
                    (Border, (0,0,0,borderRadius,1)) :
                    [ (DNA (entity2dna k), (extraData k,x,y,s,f)) | (k,(((x,y),s),f)) <- p ] ++
                    [ (Mouse, (mouseExtraData, mx, my, borderRadius/4, 1)) ]
            let stillAnimating = Presentation.Animating True
            return (DrawResult {..})
        , onMouseDown = \_ _ -> pure ()
        , onMove      = \_ _ -> pure ()
        , onMouseUp   = \_ -> pure ()
        , onMouseOut  = \_ -> pure ()
        , onDel       = \_ -> pure ()
        , onAnim      = \_ -> pure ()
        , onSave      = \_ -> pure Nothing
        , onResize = \t size -> do
            liftIO $ writeIORef sizeRef size
            -- This is a problem: resizing completely messes up with ongoing scripted interaction.
            -- Possible solution: The tutorial animation mouse movement is just for show,
            -- and it generates abstract Logic events instead.
            -- So replay the whole animation with the new screen size
            liftIO $ writeIORef asRef $ initial mealy
            liftIO $ Presentation.resetRef pRef
            resetDrag
            liftIO $ writeIORef scriptRef Tut.tutorial
            liftIO $ writeIORef scriptStepStartRef t0

            handleCmds t0 (reconstruct mealy as0)
            getPosOf t0 Tut.Center >>= liftIO . writeIORef lastMousePosition
            liftIO $ readIORef lastMousePosition >>= writeIORef currentMousePos
            liftIO $ writeIORef mouseDownRef False
            -- And now replay
            tickAnimation t
        }
