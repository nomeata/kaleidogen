module DNA where

import Control.Monad.Random.Strict
import Data.Hashable
import Data.List

type DNA = [Int]

crossover :: DNA -> DNA -> DNA
crossover x' y' = evalRand (crossover' x y) $ mkStdGen (hash (x,y))
  where [x, y] = sort [x', y']

crossover' :: MonadRandom m => DNA -> DNA -> m DNA
crossover' x y
    | null x
    = return y
    | null y
    = return x
    | length x + length y <= 2
    = do n <- getRandomR (0,30)
         return $ x ++ y ++ [n]
    | otherwise
    = w [ (pure $ x ++ y ++ [sum x + sum y], 5)
        , (pure $ zipWith' (+) x y, fromIntegral $ length x + length y)
        ]
  where
    w = join . weighted

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' op xs ys | length xs < length ys = zipWith op (cycle xs) ys
                  | otherwise             = zipWith op xs (cycle ys)

blankDNA :: DNA
blankDNA = []

initialDNAs :: [DNA]
initialDNAs = [ [x] | x <- [0..8] ]
