module Mealy where

data Mealy s e c = Mealy
    { initial :: s
    , reconstruct :: s -> c
    , handle :: s -> e -> (s, c)
    }
