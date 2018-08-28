module RNA where

import Data.Colour.RGBSpace.HSV

data RNA
    = Solid (RGB Double)
    | Blend Double RNA RNA
    | Checker Double RNA RNA
    | Rotate Double RNA
    | Invert RNA
    | Swirl Double RNA
    | Rays Int RNA RNA
    | Gradient RNA RNA
    | Ontop Double RNA RNA
    deriving Show
