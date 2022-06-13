{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Logic (
    Entity(..),
    AbstractPos(..),
    AppState(..),
    Event(..),
    logicMealy,
    isSelected, isInactive, entity2dna, selectedDNA,
    canDrag,
    ) where

import Control.Applicative
import qualified Data.Map as M
import Data.List

import DNA
import PresentationCmds (Cmds, Cmd, Cmd'(..))
import Drag (ClickEvent(..))
import Mealy

-- Lets keep the keys separate from the sequential indices
newtype Key = Key Int deriving (Num, Eq, Ord, Enum)

type Seed = Int

data AppState = AppState
    { seed :: Seed
    , dnas :: M.Map Key DNA
    , drag :: Maybe DNA
    , dragOn :: Maybe DNA
    , sel :: Maybe Key
    }

-- Events passed on to the presentation layer
data AbstractPos
    = MainPos            -- ^ The big one on top
    | SmallPos Int Int   -- ^ A little position. Parameters are count and index
    | DeletedPos Int Int -- ^ A little position, for the ghost of a just deleted element

-- The main instance can be selected, the preview instance not
data Entity = PreviewInstance DNA | MainInstance DNA
    deriving (Eq, Ord, Show)

entity2dna :: Entity -> DNA
entity2dna (MainInstance d) = d
entity2dna (PreviewInstance d) = d

dnaAtKey :: AppState -> Key -> DNA
dnaAtKey AppState{..} k = dnas M.! k

newDNA :: AppState -> Maybe DNA
newDNA AppState{..}
    | Just x <- drag, Just y <- dragOn
    = Just $ crossover seed x y
    | otherwise = Nothing

selectedDNA :: AppState -> Maybe DNA
selectedDNA as@AppState{..}
    | Just x <- sel = Just $ as `dnaAtKey` x
    | otherwise = Nothing

preview :: AppState -> Maybe DNA
preview as = newDNA as <|> selectedDNA as

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

moveAllSmall :: AppState -> Cmds Entity AbstractPos
moveAllSmall as =
    [ (MainInstance d, MoveTo (SmallPos c n))
    | (n, d) <- zip [0..] (M.elems (dnas as))
    ]
  where
    c = length (dnas as)

moveOneSmall :: AppState -> DNA -> Cmd Entity AbstractPos
moveOneSmall as d = (MainInstance d, MoveTo (SmallPos c n))
  where
    c = length (dnas as)
    Just n = elemIndex d (M.elems (dnas as))

moveMain :: AppState -> Cmds Entity AbstractPos
moveMain as =
    [ (PreviewInstance d, MoveTo MainPos)
    | Just d <- return $ newDNA as ]

data Event
    = ClickEvent (ClickEvent Entity)
    | Delete
    | Anim

logicMealy :: Seed -> Mealy AppState Event (Cmds Entity AbstractPos)
logicMealy seed = Mealy
    { initial = as0
    , reconstruct = \as -> moveAllSmall as ++ moveMain as
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
canDrag _ (MainInstance _) = True
canDrag _ _ = False

handleLogic :: AppState -> Event -> (AppState, Cmds Entity AbstractPos)
handleLogic as@AppState{..} e = case e of
    -- Changing selection
    ClickEvent (Click (MainInstance d))
        | not (isInactive as d)
        , (k:_) <- [ k | (k,d') <- M.toList dnas, d == d' ]
        -> let as' = as { sel = Just k, drag = Nothing }
           in
           ( as'
           , [ (PreviewInstance d', Remove)
             | Just d' <- [preview as]
             ] ++
             [ (PreviewInstance d', SummonAt MainPos)
             | Just d' <- [preview as']
             ]
           )

    -- Beginning a drag-and-drop action
    ClickEvent (BeginDrag (MainInstance d))
        -> ( as { drag = Just d, sel = Nothing }
           , [ (PreviewInstance (as `dnaAtKey` k_old), Remove)
             | Just k_old <- [sel]
             ]
           )

    ClickEvent (DragOn (MainInstance d'))
        | Just _ <- drag
        , not (isInactive as d')
        , let as' = as { dragOn = Just d' }
        -> ( as', [ (PreviewInstance new, SummonAt MainPos) | Just new <- [newDNA as']] )

    ClickEvent (DragOff (MainInstance _))
        | Just _ <- drag
        , let as' = as { dragOn = Nothing }
        -> ( as', [ (PreviewInstance new, Remove) | Just new <- [newDNA as]] )

    ClickEvent EndDrag
        -- Adding a new pattern
        | Just _ <- drag
        , Just new <- newDNA as
        , new `notElem` M.elems dnas -- should always be true, due to isInactive
        , let newKey = succ (fst (M.findMax dnas))
        , let dnas' = M.insert newKey new dnas
        , let as' = as { drag = Nothing, dragOn = Nothing, dnas = dnas' }
        -> ( as'
           , [ (PreviewInstance new, Remove)
             , (MainInstance new, SummonAt MainPos)
             ] ++
             moveAllSmall as'
           )

        | Just d <- drag
        -> ( as { drag = Nothing }, [ moveOneSmall as d ] )

    ClickEvent CancelDrag
        | Just d <- drag
        -> ( as { drag = Nothing }
           , [ (PreviewInstance new, Remove) | Just new <- [newDNA as]] ++
             [ moveOneSmall as d ] )

    Delete
        | Just k <- sel
        , Just i <- M.lookupIndex k dnas
        , let d = as `dnaAtKey` k
        , let as' = as { sel = Nothing, dnas = M.delete k dnas }
        -> ( as'
           , [ (PreviewInstance d, Remove)
             , (MainInstance d, FadeOut (DeletedPos (length dnas) i))
             ] ++
             moveAllSmall as'
           )

    Anim
        | Just k <- sel
        , let d = as `dnaAtKey` k
        -> ( as
           , [ (PreviewInstance d, Animate) ]
           )

    _ -> (as, [])

