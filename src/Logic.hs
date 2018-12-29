{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Logic where

import Data.Maybe
import Control.Applicative
import qualified Data.Map as M

import DNA
import qualified SelectTwo as S2

-- Lets keep the keys separate from the sequential indices
newtype Key = Key Int deriving (Num, Eq, Ord, Enum)

type Seed = Int

animationSpeed :: Double
animationSpeed = 200

data AppState = AppState
    { seed :: Seed
    , canvasSize :: (Double, Double)
    , dnas :: M.Map Key SmallDNA
    , sel :: S2.SelectTwo Key
    }

data SmallDNA = SmallDNA
    { dna :: DNA
    , added :: [Time]
    , deleted :: Maybe Time
    }

type Time = Double

initialAppState :: Seed -> AppState
initialAppState seed = AppState {..}
  where
    canvasSize = (1000, 1000)
    dnas = M.fromList $ zipWith (\n d -> (n,SmallDNA d [-1e10] Nothing)) [0..] initialDNAs
    sel = S2.duolton 0 1

dnaAtKey :: AppState -> Key -> DNA
dnaAtKey AppState{..} k = dna (dnas M.! k)

at :: AppState -> Int -> SmallDNA
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

data Event = ClickMain | ClickSmall Int | Delete

handle :: AppState -> Event -> Time -> AppState
handle as@AppState{..} e t = cleanup t $ case e of
    ClickMain
        | Just new <- newDNA as, new `notElem` map dna (M.elems dnas)
        -> as { sel = S2.empty
              , dnas = M.insert newKey (SmallDNA new [t] Nothing) dnas }
    ClickMain
        | Just new <- newDNA as
        -> let dnas' = flip fmap dnas $ \sd ->
                 if | dna sd == new -> sd { added = t : added sd }
                    | otherwise     -> sd
           in as { sel = S2.empty, dnas = dnas' }
    ClickSmall n
        | n < M.size dnas
        , isNothing (deleted (as `at` n))
        -> as { sel = S2.flip sel (as `keyAt` n) }
    Delete
        | S2.OneSelected k <- sel
        , isNothing (deleted (dnas M.! k))
        -> as { sel = S2.empty
              , dnas = M.insert k ((dnas M.! k) { deleted = Just t }) dnas }
    _ -> as
  where newKey = succ (fst (M.findMax dnas))

cleanup :: Double -> AppState -> AppState
cleanup t as =
    as { dnas = M.mapMaybe go (dnas as) }
  where
    go sd | isDeleted sd = Nothing
          | otherwise = Just $ sd { added = prune (added sd) }

    prune [] = [] -- should not happen
    prune [t'] = [t'] -- we need to keep the creation time
    prune (t':ts) | stillRelevant t' = t' : prune ts
                  | otherwise        = prune ts
    stillRelevant t' = (t - t') < animationSpeed

    isDeleted sd | Just t' <- deleted sd = not $ stillRelevant t'
                 | otherwise             = False

preview :: AppState -> Maybe DNA
preview as = newDNA as <|> selectedDNA as
