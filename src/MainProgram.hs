{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module MainProgram (mainProgram) where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad.Ref
import Data.Maybe

import Program
import Shapes
import DNA
import Layout
import Logic
import qualified Presentation
import Drag
import qualified Tutorial as Tut

toFilename :: DNA -> T.Text
toFilename dna = "kaleidogen-" <> dna2hex dna <> ".png"

layoutFun :: (Double, Double) -> AbstractPos -> PosAndScale
layoutFun size MainPos
    = topHalf (padding layoutFullCirlce) size ()
layoutFun size (SmallPos c n)
    = bottomHalf (padding (layoutGrid c)) size n
layoutFun size DeletedPos
    = topHalf (padding layoutCenterDot) size ()

mainProgram :: MonadRef m => Program m
mainProgram mbst seed0 t0 size0 = do

    let as0 = case mbst of
            Just s | Just as <- deserialize s -> as
            _ -> initialLogicState seed0
    asRef <- newRef as0
    sizeRef <- newRef size0
    pRef <- Presentation.initPRef
    let handleCmds t cs = do
          lf <- layoutFun <$> readRef sizeRef
          Presentation.handleCmdsRef t lf cs pRef
    handleCmds t0 (reconstruct as0)

    let handleEvent t e = do
          as <- readRef asRef
          let (as', cs) = handleLogic as e
          writeRef asRef as'
          handleCmds t cs

    let getPresentation t = Presentation.presentAtRef t pRef

    let canDragM k = do
          as <- readRef asRef
          return (Logic.canDrag as k)

    (dragHandler, getModPres, _resetDrag) <- mkDragHandler canDragM getPresentation

    let handleClickEvent t re = do
          dragHandler t re >>= mapM_ (handleEvent t . ClickEvent)

    return $ Callbacks
        { onDraw = \t -> do
            as <- readRef asRef
            let canDelete = isJust (sel as)
            let canSave   = isJust (sel as)
            let canAnim   = isJust (sel as)
            let canReset  = dnas as /= dnas (initialLogicState (seed as))
            (p, stillAnimating, stillVideoPlaying) <- getModPres t
            -- Calcualting the border radius
            size <- readRef sizeRef
            -- A bit of a hack to access as here
            let borderRadius = gridBorderRadius (M.size (dnas as)) size
            let objects =
                    (borderGraphic borderRadius) :
                    [ dnaGraphic d (not (isInactive as d)) pas f
                    | (k,(pas,f)) <- p , let d = entity2dna k ]
            let animInProgress = stillVideoPlaying == Presentation.VideoPlaying True
            let tutInProgress = False
            let mainDNA = head $
                    maybeToList (selectedDNA as) <>
                    maybeToList (snd <$> M.lookupMax (dnas as)) <>
                    pure []
            return (DrawResult {..})
        , onSerialize = serialize <$> readRef asRef
        , onMouseDown = \t -> handleClickEvent t . MouseDown
        , onMove = \t -> handleClickEvent t . Move
        , onMouseUp = \t -> handleClickEvent t MouseUp
        , onMouseOut = \t -> handleClickEvent t MouseOut
        , onDel = \t -> handleEvent t Delete
        , onAnim = \t -> handleEvent t Anim
        , onSave = \_ -> do
            as <- readRef asRef
            pure $ (\dna -> (toFilename dna, fullDNAGraphic dna (1000,1000))) <$> selectedDNA as
        , onResize = \t size -> do
            writeRef sizeRef size
            as <- readRef asRef
            handleCmds t (reconstruct as)
        , onReset = \t seed -> handleEvent t (Reset seed)
        , onTut = \_ -> pure ()
        , resolveDest = \ t -> \case
            (Tut.DNA n v) -> do
              as <- readRef asRef
              case M.lookup (Key n) (dnas as) of
                Just d ->  do
                  (p, _, _) <- getModPres t
                  let Just (((x,y),s),_f) = L.lookup d p
                  case v of
                    Tut.NE -> pure (x + s/5,y - s/5)
                    Tut.W  -> pure (x - s/4,y)
                -- This should not happen if the tutorial script runs through nicely
                -- but we also do not want to crash
                Nothing ->  pure (0,0)
            Tut.Center -> do
              (w,h) <- readRef sizeRef
              pure (w/2, h/2)
        }
