module DNA where

import Control.Monad.Random.Strict
import Data.Hashable


type DNA = [Int]

crossover :: DNA -> DNA -> DNA
crossover x y
    | null x = map (+1) y
    | null y = map (+2) x
    | length x + length y <= 2 = x ++ y ++ [sum x + sum y]
    | otherwise
    = flip evalRand initG $ (join . weighted)
        [ (pure $ x ++ y ++ [sum x + sum y], 1)
        , (pure $ zipWith (+) x y, 1)
        ]
  where
    initG = mkStdGen (hash (x,y))

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

