{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Program
  ( BackendRunner
  , Callbacks(..)
  , Backend(..)
  , renderGraphic
  , mainProgram
  , Graphic
  , showFullDNA
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Control.Monad.IO.Class
import Data.Foldable
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

reorderExtraData :: ((x, a), (((b,c),d),e)) -> (x, (a, b, c, d, e))
reorderExtraData ((d,b),(((x,y),s),e)) = (d, (b, x, y, s, e))

toFilename :: DNA -> T.Text
toFilename dna = "kaleidogen-" <> dna2hex dna <> ".png"

layoutFun :: (Double, Double) -> AbstractPos -> PosAndScale
layoutFun size MainPos
    = topHalf (padding layoutFullCirlce) size ()
layoutFun size (SmallPos c n)
    = bottomHalf (padding (layoutGrid False c)) size n
layoutFun size (DeletedPos c n)
    = bottomHalf (padding (layoutGrid True c)) size n


showFullDNA :: DNA -> (Double,Double) -> (Graphic, ExtraData)
showFullDNA dna (w,h) =
    reorderExtraData ((DNA dna,0), (layoutFullCirlce (w,h) (), 1))

data Backend m a = Backend
    { setCanDelete :: Bool -> m ()
    , setCanSave :: Bool -> m ()
    , setCanAnim :: Bool -> m ()
    , currentWindowSize :: m (Double,Double)
    , getCurrentTime :: m Double
    , doSave :: Text -> (a,ExtraData) -> m ()
    }

data Callbacks m a = Callbacks
    { onDraw :: m ([(a,ExtraData)], Animating)
    , onMouseDown :: (Double,Double) -> m ()
    , onMove :: (Double,Double) -> m ()
    , onMouseUp :: m ()
    , onMouseOut :: m ()
    , onDel :: m ()
    , onSave :: m ()
    , onAnim :: m ()
    , onResize :: (Double,Double) -> m ()
    }

type BackendRunner m = forall a.
    Ord a =>
    (a -> Shaders) ->
    (Backend m a -> m (Callbacks m a)) ->
    m ()

data Graphic = DNA DNA | Border deriving (Eq, Ord)

renderGraphic :: Graphic -> (Text, Text)
renderGraphic (DNA d) = (circularVertexShader, toFragmentShader (dna2rna d))
renderGraphic Border = borderShaders


mainProgram :: MonadIO m => Backend m Graphic -> m (Callbacks m Graphic)
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

    let canDragM k = do
          as <- liftIO (readIORef asRef)
          return (Logic.canDrag as k)

    (dragHandler, getModPres) <- mkDragHandler canDragM getPresentation

    let handleClickEvents re = do
          t <- getCurrentTime
          dragHandler t re >>= mapM_ (handleEvent . ClickEvent)

    return $ Callbacks
        { onDraw = do
            t <- getCurrentTime
            as <- liftIO $ readIORef asRef
            setCanDelete (isJust (sel as))
            setCanSave (isJust (sel as))
            setCanAnim (isJust (sel as))
            (p, borderRadius, continue) <- getModPres t
            let extraData d
                  -- | isSelected as d = 2
                  | isInactive as d = 3
                  | otherwise       = 0
            let toDraw =
                    (Border, (0,0,0,borderRadius,1)) :
                    [ (DNA (entity2dna k), (extraData k,x,y,s,f)) | (k,(((x,y),s),f)) <- p ]
            return (toDraw, continue)
        , onMouseDown = handleClickEvents . MouseDown
        , onMove = handleClickEvents . Move
        , onMouseUp = handleClickEvents MouseUp
        , onMouseOut = handleClickEvents MouseOut
        , onDel = handleEvent Delete
        , onAnim = handleEvent Anim
        , onSave = do
            as <- liftIO (readIORef asRef)
            for_ (selectedDNA as) $ \dna ->
                doSave (toFilename dna) (showFullDNA dna (1000,1000))
        , onResize = \size -> do
            liftIO $ writeIORef sizeRef size
            as <- liftIO $ readIORef asRef
            handleCmds (reconstruct mealy as)
        }
