{-# OPTIONS_GHC -Wno-name-shadowing #-}
module DNA
    ( DNA
    , dna2hex
    , crossover
    , Control.Monad.Random.Strict.getRandom
    , blankDNA
    , initialDNAs
    ) where

import Control.Monad.Random.Strict
import Data.Hashable
import Data.List
import Data.Word
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Text.Hex

type DNA = [Word8]

dna2hex :: DNA -> T.Text
dna2hex = encodeHex . BS.pack

crossover :: Int -> DNA -> DNA -> DNA
crossover seed x' y' =
    evalRand (crossover' x y) $ mkStdGen (hash (seed,x,y))
  where
    [x, y] = sortOn hash [x', y']

crossover' :: MonadRandom m => DNA -> DNA -> m DNA
crossover' [] dna2 = pure dna2
crossover' dna1 [] = pure dna1
crossover' dna1 dna2 =
    (\a b c -> a:b:c) <$> uniform cols1 <*> uniform cols2 <*> mixRest
  where
    (cols1,rest1) = splitAt 2 dna1
    (cols2,rest2) = splitAt 2 dna2
    -- cols = cols1 ++ cols2

    -- mixRest :: m [Word8]
    mixRest = do
        let all = rest1 ++ rest2
        let p = min 1 (exp (- fromIntegral (length all - 2) / 20) :: Double)
        new <- getRandom
        (with p (new:) id) <*> (traverse mutate =<< selectWithProb p all)

mutate :: MonadRandom m => Word8 -> m Word8
mutate x = do
    d <- uniform [1,2,4, 12,14,15]
    let x' = 16 * (x `div` 16) + (x + d) `mod` 16
    with 0.9 x x'

selectWithProb :: MonadRandom m => Double -> [a]-> m [a]
selectWithProb _ [] = pure []
selectWithProb p (x:r) =
    join $ with p
        ((x:) <$> selectWithProb p r)
        (selectWithProb p r)

with :: MonadRandom m => Double -> a -> a -> m a
with p a1 a2 = do
    p' <- getRandom
    if p' < p then pure a1 else pure a2

    {-
    (=:) = (,)
    infixr 0 =:
    w = join . weighted . map swap
    _oneOf a b = w [ 1 =: pure a, 1 =: pure b]
    -}

    {-
    mixArg n1 n2 = w
        [ 1 =: pure $ (n1 `div` 2 + n2 `div` 2)
        , 1 =: pure $ (n1 + n2)
        ]
    mixOp n1 n2 = w
        [ 2 =: pure $ n1
        , 2 =: pure $ n2
        , 1 =: pure $ n1 + n2
        ]
    -}

blankDNA :: DNA
blankDNA = [0]

initialDNAs :: [DNA]
initialDNAs = [ [x] | x <- [0,43..255] ]
