module RNA where

import Data.Colour

data Op0 = Solid (Colour Double)
         | Gradient

data Op1 = Inv
         | Swirl Double

data Op2 = Before
         | Checker
         | Rays Int

data Op3 = Blur

data RNA
    = Op0 Op0
    | Op1 Op1 RNA
    | Op2 Op2 RNA RNA
    | Op3 Op3 RNA RNA RNA
