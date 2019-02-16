{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Logic where

import Control.Applicative
import qualified Data.Map as M
import Data.List

import DNA
import qualified SelectTwo as S2

-- Lets keep the keys separate from the sequential indices
newtype Key = Key Int deriving (Num, Eq, Ord, Enum)

type Seed = Int

animationSpeed :: Double
animationSpeed = 200

data AppState = AppState
    { seed :: Seed
    , dnas :: M.Map Key DNA
    , drag :: Maybe Key
    , sel :: S2.SelectTwo Key
    }

-- Events passed on to the presentation layer
data AbstractPos = MainPos | SmallPos Int Int | DeletedPos Int Int
-- The main instance can be selected, the preview instance not
data CmdKey = PreviewInstance DNA | MainInstance DNA deriving (Eq, Ord)

key2dna :: CmdKey -> DNA
key2dna (MainInstance d) = d
key2dna (PreviewInstance d) = d

data Cmd
    = SummonAt CmdKey AbstractPos
    | MoveTo CmdKey AbstractPos
    | FadeOut CmdKey AbstractPos
    | ShiftPos CmdKey (Double, Double)
    | Remove CmdKey
type Cmds = [Cmd]

initialAppState :: Seed -> AppState
initialAppState seed = AppState {..}
  where
    dnas = M.fromList $ zipWith (\n d -> (n,d)) [0..] initialDNAs
    sel = S2.duolton 0 1
    drag = Nothing


dnaAtKey :: AppState -> Key -> DNA
dnaAtKey AppState{..} k = dnas M.! k

at :: AppState -> Int -> DNA
at AppState{..} n = snd (M.elemAt n dnas)

keyAt :: AppState -> Int -> Key
keyAt AppState{..} n = fst (M.elemAt n dnas)

newDNA :: AppState -> Maybe DNA
newDNA as@AppState{..}
    | S2.TwoSelected x y <- sel
    = Just $ crossover seed (as `dnaAtKey` x) (as `dnaAtKey` y)
    | otherwise = Nothing

selectedDNA :: AppState -> Maybe DNA
selectedDNA as@AppState{..}
    | S2.OneSelected x <- sel = Just $ as `dnaAtKey` x
    | otherwise = Nothing

preview :: AppState -> Maybe DNA
preview as = newDNA as <|> selectedDNA as

isSelected :: AppState -> DNA -> Bool
isSelected as@AppState{..} = case sel of
    S2.TwoSelected x y -> \d -> d `elem` [as `dnaAtKey` x, as `dnaAtKey` y]
    S2.OneSelected x -> \d -> d == (as `dnaAtKey` x)
    _ -> const False

moveAllSmall :: AppState -> Cmds
moveAllSmall as =
    [ MoveTo (MainInstance d) (SmallPos c n)
    | (n, d) <- zip [0..] (M.elems (dnas as))
    ]
  where
    c = length (dnas as)

moveOneSmall :: AppState -> DNA -> Cmd
moveOneSmall as d = MoveTo (MainInstance d) (SmallPos c n)
  where
    c = length (dnas as)
    Just n = elemIndex d (M.elems (dnas as))

moveMain :: AppState -> Cmds
moveMain as =
    [ MoveTo (PreviewInstance d) MainPos
    | Just d <- return $ newDNA as ]

initialCommands :: AppState -> Cmds
initialCommands as = moveAllSmall as ++ moveMain as

data Event
    = Click CmdKey
    | BeginDrag CmdKey
    | DragDelta (Double, Double)
    | EndDrag
    | Delete

handle :: AppState -> Event -> (AppState, Cmds)
handle as@AppState{..} e = case e of
    -- Adding a new pattern
    Click (PreviewInstance d)
        | Just new <- newDNA as, d == new
        , new `notElem` M.elems dnas
        , let newKey = succ (fst (M.findMax dnas))
        , let dnas' = M.insert newKey new dnas
        , let as' = as { sel = S2.empty, dnas = dnas' }
        -> ( as'
           , [ Remove (PreviewInstance new)
             , SummonAt (MainInstance new) MainPos
             ] ++
             moveAllSmall as'
           )
    -- Clicking an already added pattern
    Click (PreviewInstance d)
        | Just new <- newDNA as, d == new
        , Just i <- elemIndex d (M.elems dnas)
        -> -- send preview move event
           ( as { sel = S2.empty, dnas = dnas }
           , [ FadeOut (PreviewInstance new) (SmallPos (length dnas) i) ] )

    Click (MainInstance d)
        | (k:_) <- [ k | (k,d') <- M.toList dnas, d == d' ]
        -> let as' = as { sel = S2.flip sel k , drag = Nothing }
           in
           ( as'
           , [ Remove (PreviewInstance d')
             | Just d' <- pure $ preview as
             ] ++
             [ SummonAt (PreviewInstance d') MainPos
             | Just d' <- pure $ preview as'
             ]
           )

    -- Changing selection
    BeginDrag (MainInstance d)
        | (k:_) <- [ k | (k,d') <- M.toList dnas, d == d' ]
        -> ( as { drag = Just k }
           , case sel of
               S2.OneSelected k_old -> [ Remove (PreviewInstance (as `dnaAtKey` k_old)) ]
               S2.TwoSelected {} | Just old <- newDNA as -> [ Remove (PreviewInstance old) ]
               _ -> []
           )

    DragDelta p
        | Just k <- drag
        , let d = as `dnaAtKey` k
        -> ( as { sel = S2.empty }, [ ShiftPos (MainInstance d) p ] )

    EndDrag
        | Just k <- drag
        , let d = as `dnaAtKey` k
        -> ( as { drag = Nothing }, [ moveOneSmall as d ] )

    Delete
        | S2.OneSelected k <- sel
        , Just i <- M.lookupIndex k dnas
        , let d = as `dnaAtKey` k
        , let as' = as { sel = S2.empty, dnas = M.delete k dnas }
        -> ( as'
           , [ Remove (PreviewInstance d)
             , FadeOut (MainInstance d) (DeletedPos (length dnas) i)
             ] ++
             moveAllSmall as'
           )
    _ -> (as, [])

