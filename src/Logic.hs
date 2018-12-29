{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
module Logic where

import DNA
import qualified SelectTwo as S2

type Seed = Int

data AppState = AppState
    { seed :: Seed
    , canvasSize :: (Double, Double)
    , dnas :: [DNA]
    , sel :: S2.SelectTwo Int
    }

initialAppState :: Seed -> AppState
initialAppState seed = AppState {..}
  where
    canvasSize = (1000, 1000)
    dnas = initialDNAs
    sel = S2.duolton 0 1

newDNA :: AppState -> Maybe DNA
newDNA AppState{..}
    | S2.TwoSelected x y <- sel = Just $ crossover seed (dnas!!x) (dnas!!y)
    | otherwise = Nothing

selectedDNA :: AppState -> Maybe DNA
selectedDNA AppState{..}
    | S2.OneSelected x <- sel = Just $ dnas!!x
    | otherwise = Nothing

data Event = ClickMain | ClickSmall Int | Delete

handle :: AppState -> Event -> AppState
handle as@AppState{..} ClickMain
    | Just new <- newDNA as, new `notElem` dnas
    =  as { sel = S2.empty, dnas = dnas ++ [new] }
handle as@AppState{..} (ClickSmall n)
    =  as { sel = S2.flip sel n }
handle as@AppState{..} Delete
    | S2.OneSelected n <- sel
    =  as { sel = S2.empty, dnas = take n dnas ++ drop (n+1) dnas }
handle as _ = as

preview :: Seed -> S2.SelectTwo DNA -> Maybe DNA
preview _    S2.NoneSelected = Nothing
preview _    (S2.OneSelected x)   = Just x
preview seed (S2.TwoSelected x y) = Just $ crossover seed x y

