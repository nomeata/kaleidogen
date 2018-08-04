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
    | length x + length y <= 2
    = do n <- getRandomR (0,30)
         return $ x ++ y ++ [n]
    | otherwise
    = w [ (pure $ x ++ y ++ [sum x + sum y], 1)
        , (pure $ zipWith' (+) x y, 1)
        ]
  where
    w = join . weighted

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' op xs ys = zipWith op xs ys ++ drop n xs ++ drop n ys
  where n = min (length xs) (length ys)

initialDNAs :: [DNA]
initialDNAs =
    [ [0]
    , [1]
    , [2]
    , [3]
    , [4]
    , [5]
    , [6]
    , [7]
    , [8]
    ]

