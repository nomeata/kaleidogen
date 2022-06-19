{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Program
  ( ProgramRunner
  , Callbacks(..)
  , Time
  , DrawResult(..)
  , renderGraphic
  , mainProgram
  , tutorialProgram
  , switchProgram
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
    , onTut       :: Time -> m ()
    , resolveDest :: Time -> Tut.Destination -> m (Double,Double)
    }

type ProgramRunner m = forall a.
    Ord a =>
    (a -> Shaders) ->
    (Int -> Time -> (Double, Double) -> m (Callbacks m a)) ->
    m ()

data Graphic = DNA DNA | Border | Mouse deriving (Eq, Ord)

renderGraphic :: Graphic -> (Text, Text)
renderGraphic (DNA d) = (circularVertexShader, toFragmentShader (dna2rna d))
renderGraphic Border = borderShaders
renderGraphic Mouse  = (circularVertexShader, mouseFragmentShader)


mainProgram :: MonadIO m => Int -> Time -> (Double, Double) -> m (Callbacks m Graphic)
mainProgram seed0 t0 size0 = do

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

    let handleClickEvent t re = do
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
        , onMouseDown = \t -> handleClickEvent t . MouseDown
        , onMove = \t -> handleClickEvent t . Move
        , onMouseUp = \t -> handleClickEvent t MouseUp
        , onMouseOut = \t -> handleClickEvent t MouseOut
        , onDel = \t -> handleEvent t Delete
        , onAnim = \t -> handleEvent t Anim
        , onSave = \_ -> do
            as <- liftIO (readIORef asRef)
            pure $ (\dna -> (toFilename dna, showFullDNA dna (1000,1000))) <$> selectedDNA as
        , onResize = \t size -> do
            liftIO $ writeIORef sizeRef size
            as <- liftIO $ readIORef asRef
            handleCmds t (reconstruct mealy as)
        , onTut = \_ -> pure ()
        , resolveDest = \ t -> \case
            (Tut.DNA n v) -> do
              as <- liftIO $ readIORef asRef
              case M.lookup (Key n) (dnas as) of
                Just d ->  do
                  (p, _continue) <- getModPres t
                  let Just (((x,y),s),_f) = L.lookup d p
                  case v of
                    Tut.NE -> pure (x + s/5,y - s/5)
                    Tut.W  -> pure (x - s/4,y)
                -- This should not happen if the tutorial script runs through nicely
                -- but we also do not want to crash
                Nothing ->  pure (0,0)
            Tut.Center -> do
              (w,h) <- liftIO $ readIORef sizeRef
              pure (w/2, h/2)
        }

-- TODO: Find new abstractions to remove duplication with above
-- TODO: Make it all pure? Or state monad?

tutorialProgram :: MonadIO m => Int -> Time -> (Double,Double) -> m (Callbacks m Graphic)
tutorialProgram seed0 t0 size0 = do
    progRef <- mainProgram seed0 t0 size0 >>= liftIO . newIORef
    let withProg act = liftIO (readIORef progRef) >>= act
    let getPosOf t d = withProg $ \p -> resolveDest p t d

    -- Script playing state
    scriptRef <- liftIO $ newIORef Tut.tutorial
    scriptStepStartRef <- liftIO $ newIORef t0
    lastMousePosition <- getPosOf t0 Tut.Center >>= liftIO . newIORef
    currentMousePos <- liftIO $ readIORef lastMousePosition >>= newIORef
    mouseDownRef <- liftIO $ newIORef False

    -- Need screen size to size the mouse
    sizeRef <- liftIO $ newIORef size0

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
                    withProg $ \p -> onMove p (t1 + n) (x,y)
                    liftIO $ writeIORef scriptStepStartRef (t1 + n)
                    liftIO $ writeIORef lastMousePosition (x,y)
                    liftIO $ writeIORef currentMousePos (x,y)
                    liftIO $ writeIORef scriptRef s'
                    tickAnimation t
                  | otherwise -> do
                    -- Mouse move is happening
                    (x,y) <- getPosOf (t1 + n) d
                    let mousePos = tween ((t-t1)/n) (x1,y1) (x,y)
                    withProg $ \p -> onMove p (t1 + n) mousePos
                    liftIO $ writeIORef currentMousePos mousePos
                Tut.MouseDown:s' -> do
                    withProg $ \p -> onMouseDown p t1 (x1, y1)
                    liftIO $ writeIORef scriptRef s'
                    liftIO $ writeIORef mouseDownRef True
                    tickAnimation t
                Tut.MouseUp:s' -> do
                    withProg $ \p -> onMouseUp p t1
                    liftIO $ writeIORef scriptRef s'
                    liftIO $ writeIORef mouseDownRef False
                    tickAnimation t
                Tut.StartAnim:s' -> do
                    withProg $ \p -> onAnim p t1
                    liftIO $ writeIORef scriptRef s'
                    tickAnimation t

    return $ Callbacks
        { onDraw = \t -> do
            tickAnimation t

            dr <- withProg $ \p -> onDraw p t

            -- Mouse pointer
            isMouseDown <- liftIO $ readIORef mouseDownRef
            let mouseExtraData
                  | isMouseDown = 1
                  | otherwise   = 0
            (mx,my) <- liftIO $ readIORef currentMousePos
            (w,h) <- liftIO $ readIORef sizeRef
            let s = min (w/30) (h/30)

            -- TODO: mouse size
            let mouseObject = (Mouse, (mouseExtraData, mx, my, s, 1))

            stillAnimating <- Presentation.Animating . not . null <$> liftIO (readIORef scriptRef)

            pure $ dr
                { objects = objects dr ++ [mouseObject]
                , stillAnimating = stillAnimating
                }

        , onMouseDown = \_ _ -> pure ()
        , onMove      = \_ _ -> pure ()
        , onMouseUp   = \_ -> pure ()
        , onMouseOut  = \_ -> pure ()
        , onDel       = \_ -> pure ()
        , onAnim      = \_ -> pure ()
        , onSave      = \_ -> pure Nothing
        , onTut       = \_ -> pure ()
        , onResize = \t size -> do
            liftIO $ writeIORef sizeRef size
            -- This is a problem: resizing completely messes up with ongoing scripted interaction.
            -- Possible solution: The tutorial animation mouse movement is just for show,
            -- and it generates abstract Logic events instead.
            -- So just replay the whole animation with the new screen size! (Using same seed)
            mainProgram seed0 t0 size >>= liftIO . writeIORef progRef
            liftIO $ writeIORef scriptRef Tut.tutorial
            liftIO $ writeIORef scriptStepStartRef t0

            getPosOf t0 Tut.Center >>= liftIO . writeIORef lastMousePosition
            liftIO $ readIORef lastMousePosition >>= writeIORef currentMousePos
            liftIO $ writeIORef mouseDownRef False
            -- And now replay
            tickAnimation t
        , resolveDest = \t d -> withProg $ \p -> resolveDest p t d
        }

switchProgram :: MonadIO m => Int -> Time -> (Double,Double) -> m (Callbacks m Graphic)
switchProgram seed0 t0 size0 = do
    mainRef <- mainProgram seed0 t0 size0 >>= liftIO . newIORef
    tutRef <- liftIO $ newIORef Nothing

    let withTut act = liftIO (readIORef tutRef) >>= \case
            Just p -> act p
            Nothing -> pure ()
    let withMain act = liftIO (readIORef mainRef) >>= act
    let withTutOrMain act = liftIO (readIORef tutRef) >>= \case
            Just p -> act p
            Nothing -> liftIO (readIORef mainRef) >>= \p -> act p
    let withTutAndMain act = withTut act >> withMain act

    let startTutorial t = do
            tutorialProgram seed0 t size0  >>= liftIO . writeIORef tutRef . Just
    let stopTutorial = liftIO $ writeIORef tutRef Nothing

    return $ Callbacks
        { onDraw      = \t      -> withTutOrMain  $ \p -> do
            dr <- onDraw p t
            case stillAnimating dr of
                -- Still running
                Presentation.Animating True -> pure dr
                -- Presentation stopped, switch to main program
                Presentation.Animating False -> do
                    stopTutorial
                    withMain $ \p' -> onDraw p' t

        , onMouseDown = \t pos  -> withTutOrMain  $ \p -> onMouseDown p t pos
        , onMove      = \t pos  -> withTutOrMain  $ \p -> onMove p t pos
        , onMouseUp   = \t      -> withTutOrMain  $ \p -> onMouseUp p t
        , onMouseOut  = \t      -> withTutOrMain  $ \p -> onMouseOut p t
        , onDel       = \t      -> withTutOrMain  $ \p -> onDel p t
        , onAnim      = \t      -> withTutOrMain  $ \p -> onAnim p t
        , onSave      = \t      -> withTutOrMain  $ \p -> onSave p t
        , onResize    = \t size -> withTutAndMain $ \p -> onResize p t size
                -- NB: We keep updating the screen size for both
        , resolveDest = \t d    -> withTutOrMain  $ \p -> resolveDest p t d
        , onTut = \t -> liftIO (readIORef tutRef) >>= \case
            -- Tutorial is not running, so
            Nothing -> do
                -- Pretend the mouse went out on the real program
                withMain $ \p -> onMouseOut p t
                startTutorial t
            -- Tutorial is running, so stop it
            Just _ -> stopTutorial
        }

