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
    isSelected, entity2dna, selectedDNA,
    ) where

import Control.Applicative
import qualified Data.Map as M
import Data.List

import DNA
import qualified SelectTwo as S2
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
    , sel :: S2.SelectTwo Key
    }

-- Events passed on to the presentation layer
data AbstractPos = MainPos | SmallPos Int Int | DeletedPos Int Int

-- The main instance can be selected, the preview instance not
data Entity = PreviewInstance DNA | MainInstance DNA
    deriving (Eq, Ord, Show)

entity2dna :: Entity -> DNA
entity2dna (MainInstance d) = d
entity2dna (PreviewInstance d) = d

dnaAtKey :: AppState -> Key -> DNA
dnaAtKey AppState{..} k = dnas M.! k

newDNA :: AppState -> Maybe DNA
newDNA as@AppState{..}
    | S2.TwoSelected x y <- sel
    = Just $ crossover seed (as `dnaAtKey` x) (as `dnaAtKey` y)
    | Just x <- drag, Just y <- dragOn
    = Just $ crossover seed x y
    | otherwise = Nothing

selectedDNA :: AppState -> Maybe DNA
selectedDNA as@AppState{..}
    | S2.OneSelected x <- sel = Just $ as `dnaAtKey` x
    | otherwise = Nothing

preview :: AppState -> Maybe DNA
preview as = newDNA as <|> selectedDNA as

isSelected :: AppState -> DNA -> Bool
isSelected as@AppState{..} = if
    | S2.TwoSelected x y <- sel -> \d -> d `elem` [as `dnaAtKey` x, as `dnaAtKey` y]
    | S2.OneSelected x <- sel -> \d -> d == (as `dnaAtKey` x)
    | Just x <- drag, Just y <- dragOn -> \d -> d `elem` [x,y]
    | otherwise -> const False

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

logicMealy :: Seed -> Mealy AppState Event (Cmds Entity AbstractPos)
logicMealy seed = Mealy
    { initial = as0
    , reconstruct = \as -> moveAllSmall as ++ moveMain as
    , handle = handleLogic
    }
  where
    as0 = AppState {..}
      where
        dnas = M.fromList $ zipWith (\n d -> (n,d)) [0..] initialDNAs
        sel = S2.duolton 0 1
        drag = Nothing
        dragOn = Nothing


handleLogic :: AppState -> Event -> (AppState, Cmds Entity AbstractPos)
handleLogic as@AppState{..} e = case e of
    -- Adding a new pattern
    ClickEvent (Click (PreviewInstance d))
        | Just new <- newDNA as, d == new
        , new `notElem` M.elems dnas
        , let newKey = succ (fst (M.findMax dnas))
        , let dnas' = M.insert newKey new dnas
        , let as' = as { sel = S2.empty, dnas = dnas' }
        -> ( as'
           , [ (PreviewInstance new, Remove)
             , (MainInstance new, SummonAt MainPos)
             ] ++
             moveAllSmall as'
           )
    -- Clicking an already added pattern
    ClickEvent (Click (PreviewInstance d))
        | Just new <- newDNA as
        , d == new
        , Just i <- elemIndex d (M.elems dnas)
        -> -- send preview move event
           ( as { sel = S2.empty, dnas = dnas }
           , [ (PreviewInstance new, FadeOut (SmallPos (length dnas) i)) ] )

    -- Changing selection
    ClickEvent (Click (MainInstance d))
        | (k:_) <- [ k | (k,d') <- M.toList dnas, d == d' ]
        -> let as' = as { sel = S2.flip sel k , drag = Nothing }
           in
           ( as'
           , [ (PreviewInstance d', Remove)
             | Just d' <- pure $ preview as
             ] ++
             [ (PreviewInstance d', SummonAt MainPos)
             | Just d' <- pure $ preview as'
             ]
           )

    -- Beginning a drag-and-drop action
    ClickEvent (BeginDrag (MainInstance d))
        -> ( as { drag = Just d, sel = S2.empty }
           , case sel of
               S2.OneSelected k_old -> [ (PreviewInstance (as `dnaAtKey` k_old), Remove ) ]
               S2.TwoSelected {} | Just old <- newDNA as -> [ (PreviewInstance old, Remove) ]
               _ -> []
           )

    ClickEvent (DragDelta p)
        | Just d <- drag
        -> ( as, [ (MainInstance d, ShiftPos p) ] )

    ClickEvent (DragOn (MainInstance d'))
        | Just _ <- drag
        , let as' = as { dragOn = Just d' }
        -> ( as', [ (PreviewInstance new, SummonAt MainPos) | Just new <- pure $ newDNA as' ] )

    ClickEvent (DragOff (MainInstance _'))
        | Just _ <- drag
        , let as' = as { dragOn = Nothing }
        -> ( as', [ (PreviewInstance new, Remove) | Just new <- pure $ newDNA as ] )

    ClickEvent EndDrag
        | Just _ <- drag
        , Just new <- newDNA as
        , new `notElem` M.elems dnas
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
        , Just new <- newDNA as
        , Just i <- elemIndex new (M.elems dnas)
        -> ( as { drag = Nothing }
           , [ (PreviewInstance new, FadeOut (SmallPos (length dnas) i))
             , moveOneSmall as d ] )

        | Just d <- drag
        -> ( as { drag = Nothing }, [ moveOneSmall as d ] )

    ClickEvent CancelDrag
        | Just d <- drag
        -> ( as { drag = Nothing }
           , [ (PreviewInstance new, Remove) | Just new <- pure $ newDNA as ] ++
             [ moveOneSmall as d ] )

    Delete
        | S2.OneSelected k <- sel
        , Just i <- M.lookupIndex k dnas
        , let d = as `dnaAtKey` k
        , let as' = as { sel = S2.empty, dnas = M.delete k dnas }
        -> ( as'
           , [ (PreviewInstance d, Remove)
             , (MainInstance d, FadeOut (DeletedPos (length dnas) i))
             ] ++
             moveAllSmall as'
           )
    _ -> (as, [])

