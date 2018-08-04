module DNA where

type DNA = [Int]

crossover :: DNA -> DNA -> DNA
crossover x y = x ++ y ++ [sum x + sum y]

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

