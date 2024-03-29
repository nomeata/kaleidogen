{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Program
  ( Program
  , Time
  , ProgramRunner
  , Callbacks(..)
  , DrawResult(..)
  , switchProgram
  )
where

import Data.Text (Text)
import Control.Monad.Ref
import Data.Int
import DNA

import Shaders
import Presentation (Animating)
import qualified Presentation
import qualified Tutorial as Tut

type Time = Double
type StoredState = String

data DrawResult = DrawResult
    { objects :: [Graphic]
    , mainDNA :: DNA
    , stillAnimating :: Animating
    , canDelete :: Bool
    , canSave :: Bool
    , canAnim :: Bool
    , canReset :: Bool
    , animInProgress :: Bool
    , tutInProgress :: Bool
    }

data Callbacks m = Callbacks
    { onDraw      :: Time -> m DrawResult
    , onSerialize :: m StoredState
    , onMouseDown :: Time -> (Double,Double) -> m ()
    , onMove      :: Time -> (Double,Double) -> m ()
    , onMouseUp   :: Time -> m ()
    , onMouseOut  :: Time -> m ()
    , onDel       :: Time -> m ()
    , onSave      :: Time -> m (Maybe (Text, Graphic))
    , onAnim      :: Time -> m ()
    , onResize    :: Time -> (Double,Double) -> m ()
    , onReset     :: Time -> Int64 -> m ()
    , onTut       :: Time -> m ()
    , resolveDest :: Time -> Tut.Destination -> m (Double,Double)
    }

type Program m = Maybe StoredState -> Int64 -> Time -> (Double, Double) -> m (Callbacks m)
type ProgramRunner m = Program m -> m ()


-- A combinator to switch to another program upon onTut
switchProgram :: MonadRef m => Program m -> Program m -> Program m
switchProgram mainP otherP st0 seed0 t0 size0 = do
    mainRef <- mainP st0 seed0 t0 size0 >>= newRef
    otherRef <- newRef Nothing

    let withOther act = readRef otherRef >>= \case
            Just p -> act p
            Nothing -> pure ()
    let withMain act = readRef mainRef >>= act
    let withOtherOrMain act = readRef otherRef >>= \case
            Just p -> act p
            Nothing -> readRef mainRef >>= \p -> act p
    let withOtherAndMain act = withOther act >> withMain act

    -- Remember screen size
    sizeRef <- newRef size0

    let startOther t = do
            size <- readRef sizeRef
            otherP Nothing seed0 t size >>= writeRef otherRef . Just
    let stopOther = writeRef otherRef Nothing


    return $ Callbacks
        { onDraw      = \t      -> withOtherOrMain  $ \p -> do
            dr <- onDraw p t
            case stillAnimating dr of
                -- Still running
                Presentation.Animating True -> pure dr
                -- Presentation stopped, switch to main program
                Presentation.Animating False -> do
                    stopOther
                    withMain $ \p' -> onDraw p' t

        -- Serialization always based on main program
        , onSerialize = withMain onSerialize

        , onMouseDown = \t pos  -> readRef otherRef >>= \case
            Nothing -> withMain $ \p -> onMouseDown p t pos
            -- Clicking during other programs ends it
            Just _  -> stopOther
        , onMove      = \t pos  -> withOtherOrMain  $ \p -> onMove p t pos
        , onMouseUp   = \t      -> withOtherOrMain  $ \p -> onMouseUp p t
        , onMouseOut  = \t      -> withOtherOrMain  $ \p -> onMouseOut p t
        , onDel       = \t      -> withOtherOrMain  $ \p -> onDel p t
        , onAnim      = \t      -> withOtherOrMain  $ \p -> onAnim p t
        , onSave      = \t      -> withOtherOrMain  $ \p -> onSave p t
        , onResize    = \t size -> withOtherAndMain $ \p -> do
            writeRef sizeRef size
            onResize p t size
                -- NB: We keep updating the screen size for both
        , resolveDest = \t d    -> withOtherOrMain  $ \p -> resolveDest p t d
        , onReset = \t s -> withOtherOrMain $ \p -> onReset p t s
        , onTut = \t -> readRef otherRef >>= \case
            -- Other is not running, so
            Nothing -> do
                -- Pretend the mouse went out on the real program
                withMain $ \p -> onMouseOut p t
                startOther t
            -- Other is running, so stop it
            Just _ -> stopOther
        }

