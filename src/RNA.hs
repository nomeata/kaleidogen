module RNA where

import Data.Colour

data RNA
    = Solid (Colour Double)
    | Blend Double RNA RNA
    | Checker Double RNA RNA
    | Rotate Double RNA
    | Invert RNA
    | Swirl Double RNA
    | Rays Int RNA RNA
    | Gradient RNA RNA
    | Ontop Double RNA RNA
    deriving Show
