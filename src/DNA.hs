module DNA where

type DNA = [Int]

crossover :: DNA -> DNA -> DNA
crossover = (++)

initialDNAs :: [DNA]
initialDNAs =
    [ [0]
    , [1]
    , [2]
    ]

