{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Logic where

import DNA
import qualified SelectTwo as S2
import Control.Applicative

type Seed = Int

data AppState = AppState
    { seed :: Seed
    , canvasSize :: (Double, Double)
    , dnas :: [(DNA, [Time])]
    , sel :: S2.SelectTwo Int
    }

type Time = Double

initialAppState :: Seed -> AppState
initialAppState seed = AppState {..}
  where
    canvasSize = (1000, 1000)
    dnas = map (,[-1e10]) initialDNAs
    sel = S2.duolton 0 1

dnaAt :: AppState -> Int -> DNA
dnaAt AppState{..} n = dna
  where (dna,_) = dnas !! n

newDNA :: AppState -> Maybe DNA
newDNA as@AppState{..}
    | S2.TwoSelected x y <- sel
    = Just $ crossover seed (as `dnaAt` x) (as `dnaAt` y)
    | otherwise = Nothing

selectedDNA :: AppState -> Maybe DNA
selectedDNA as@AppState{..}
    | S2.OneSelected x <- sel = Just $ as `dnaAt` x
    | otherwise = Nothing

data Event = ClickMain | ClickSmall Int | Delete

handle :: AppState -> Event -> Time -> AppState
handle as@AppState{..} ClickMain t
    | Just new <- newDNA as, new `notElem` map fst dnas
    =  as { sel = S2.empty, dnas = dnas ++ [(new, [t])] }
    | Just new <- newDNA as
    = let dnas' = flip map dnas $ \case
            (d,ts) | d == new  -> (d,t:ts)
                   | otherwise -> (d,ts)
      in as { sel = S2.empty, dnas = dnas' }

handle as@AppState{..} (ClickSmall n) _t
    =  as { sel = S2.flip sel n }
handle as@AppState{..} Delete _t
    | S2.OneSelected n <- sel
    =  as { sel = S2.empty, dnas = take n dnas ++ drop (n+1) dnas }
handle as _ _ = as

preview :: AppState -> Maybe DNA
preview as = newDNA as <|> selectedDNA as
