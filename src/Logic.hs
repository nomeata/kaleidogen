{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Logic (
    Entity,
    AbstractPos(..),
    AppState(..),
    Event(..),
    logicMealy,
    isSelected, isInactive, entity2dna, selectedDNA,
    canDrag,
    ) where

import qualified Data.Map as M
import Data.List

import DNA
import PresentationCmds (Cmds, Cmd, Cmd'(..))
import Drag (ClickEvent(..))
import Mealy

-- Lets keep the keys separate from the sequential indices
newtype Key = Key Int deriving (Num, Eq, Ord, Enum, Show)

type Seed = Int

data AppState = AppState
    { seed :: Seed
    , dnas :: M.Map Key DNA
    , drag :: Maybe DNA
    , dragOn :: Maybe DNA
    , sel :: Maybe Key
    }
  deriving Show

-- Events passed on to the presentation layer
data AbstractPos
    = MainPos            -- ^ The big one on top
    | SmallPos Int Int   -- ^ A little position. Parameters are count and index
    | DeletedPos         -- ^ The big one, for the shrinking of deleted ones
  deriving (Show, Eq)

type Entity = DNA

entity2dna :: Entity -> DNA
entity2dna = id

dnaAtKey :: AppState -> Key -> DNA
dnaAtKey AppState{..} k = dnas M.! k

newDNA :: AppState -> Maybe DNA
newDNA AppState{..}
    | Just x <- drag, Just y <- dragOn = Just $ crossover seed x y
    | otherwise = Nothing

selectedDNA :: AppState -> Maybe DNA
selectedDNA as@AppState{..}
    | Just x <- sel = Just $ as `dnaAtKey` x
    | otherwise = Nothing

isInactive :: AppState -> DNA -> Bool
isInactive as@AppState{..} = if
    | Just d <- drag  -> alreadyCombinedWith as d
    | otherwise       -> const False

isSelected :: AppState -> DNA -> Bool
isSelected as@AppState{..} = if
    | Just x <- sel                    -> \d -> d == (as `dnaAtKey` x)
    | Just x <- drag, Just y <- dragOn -> \d -> d `elem` [x,y]
    | otherwise                        -> const False

alreadyCombinedWith :: AppState -> DNA -> DNA -> Bool
alreadyCombinedWith as d1 d2 =
    -- Is this too slow, recombining them all the time?
    crossover (seed  as) d1 d2 `elem` M.elems (dnas as)

moveAll :: AppState -> Cmds Entity AbstractPos
moveAll as =
    [ if Just d == selectedDNA as
      then (d, MoveTo MainPos)
      else (d, MoveTo (SmallPos c n))
    | (n, d) <- zip [0..] (M.elems (dnas as))
    ] ++
    [ (d, MoveTo MainPos) | Just d <- [newDNA as] ]
  where
    c = length (dnas as)

moveOneSmall :: AppState -> DNA -> Cmd Entity AbstractPos
moveOneSmall as d = (d, MoveTo (SmallPos c n))
  where
    c = length (dnas as)
    Just n = elemIndex d (M.elems (dnas as))

data Event
    = ClickEvent (ClickEvent Entity)
    | Delete
    | Anim
  deriving Show

logicMealy :: Seed -> Mealy AppState Event (Cmds Entity AbstractPos)
logicMealy seed = Mealy
    { initial = as0
    , reconstruct = \as -> moveAll as
    , handle = handleLogic
    }
  where
    as0 = AppState {..}
      where
        dnas = M.fromList $ zip [0..] initialDNAs
        sel = Nothing
        drag = Nothing
        dragOn = Nothing


canDrag :: AppState -> Entity -> Bool
canDrag _ _ = True

handleLogic :: AppState -> Event -> (AppState, Cmds Entity AbstractPos)
handleLogic as@AppState{..} e = case e of
    -- Changing selection
    ClickEvent (Click d)
        | not (isInactive as d)
        , Just d /= ((as `dnaAtKey`) <$> sel)
        , Nothing <- drag
        , (k:_) <- [ k | (k,d') <- M.toList dnas, d == d' ]
        , let as' = as { sel = Just k, drag = Nothing }
        -> ( as'
           , [ moveOneSmall as' (as `dnaAtKey` k_old) | Just k_old <- [sel] ] ++
             [ (d, MoveTo MainPos) ]
           )

    -- Beginning a drag-and-drop action
    ClickEvent (BeginDrag d)
        | Nothing <- drag
        , let as' = as { drag = Just d, sel = Nothing }
        -> ( as'
           , [ moveOneSmall as' (as `dnaAtKey` k_old) | Just k_old <- [sel] ]
           )

    ClickEvent (DragOn d')
        | Just d <- drag
        , Nothing <- dragOn
        , d' /= d                -- Should not happen
        , not (isInactive as d') -- Should not happen
        , let as' = as { dragOn = Just d' }
        -> ( as', [ (new, SummonAt MainPos) | Just new <- [newDNA as']] )

    ClickEvent (DragOff _)
        | Just _ <- drag
        , let as' = as { dragOn = Nothing }
        -> ( as', [ (new, Remove) | Just new <- [newDNA as]] )

    ClickEvent EndDrag
        -- Adding a new pattern
        | Just _ <- drag
        , Just new <- newDNA as
        , new `notElem` M.elems dnas -- should always be true, due to isInactive
        , let newKey = succ (fst (M.findMax dnas))
        , let dnas' = M.insert newKey new dnas
        , let as' = as { drag = Nothing, dragOn = Nothing, dnas = dnas' }
        -> ( as'
           , moveAll as'
           )

        | Just d <- drag
        -> ( as { drag = Nothing, dragOn = Nothing }, [ moveOneSmall as d ] )

    ClickEvent CancelDrag
        | Just d <- drag
        -> ( as { drag = Nothing, dragOn = Nothing }
           , [ (new, Remove) | Just new <- [newDNA as]] ++
             [ moveOneSmall as d ] )

    -- Fall-through: Unselect if necessary
    ClickEvent (Click _)
        | Just k <- sel
        , let as' = as { sel = Nothing }
        -> (as', [moveOneSmall as' (as `dnaAtKey` k)])
    ClickEvent OtherClick
        | Just k <- sel
        , let as' = as { sel = Nothing }
        -> (as', [moveOneSmall as' (as `dnaAtKey` k)])


    Delete
        | Just k <- sel
        , let d = as `dnaAtKey` k
        , let as' = as { sel = Nothing, dnas = M.delete k dnas }
        -> ( as'
           , [ (d, FadeOut DeletedPos) ] ++
             moveAll as'
           )

    Anim
        | Just k <- sel
        , let d = as `dnaAtKey` k
        -> ( as
           , [ (d, Animate) ]
           )

    _ -> (as, [])

