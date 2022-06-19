-- The abstract script of the tutorial
module Tutorial
  ( Script
  , ScriptStep(..)
  , Delay
  , Destination(..)
  , Variation(..)
  , tutorial
  ) where

import Control.Monad.Writer

type Script = [ScriptStep]
type ScriptM = Writer Script ()
type Delay = Double -- in milliseconds

data Destination
    = Center
    | DNA Int Variation

data Variation = NE | W

data ScriptStep
    = Wait Delay
    | MoveMouseTo Destination Delay
    | MouseDown
    | MouseUp
    | StartAnim


t :: ScriptStep -> ScriptM
t = tell . (:[])

wait :: Delay -> ScriptM
wait delay             = t $ Wait delay
moveMouseTo :: Destination -> Delay -> ScriptM
moveMouseTo dest delay = t $ MoveMouseTo dest delay
mouseDown :: ScriptM
mouseDown              = t MouseDown
mouseUp :: ScriptM
mouseUp                = t MouseUp
startAnim :: ScriptM
startAnim              = t StartAnim

click :: Destination -> ScriptM
click d = moveMouseTo d 500 >> wait 100 >> mouseDown >> wait 200 >> mouseUp

dragOnto :: Destination -> Destination -> ScriptM
dragOnto d1 d2 = do
  moveMouseTo d1 500 >> wait 500 >> mouseDown >> wait 200
  moveMouseTo d2 1000 >> wait 1000 >> mouseUp

dragOntoVia :: Destination -> Destination -> Destination -> ScriptM
dragOntoVia d1 d2 d3 = do
  moveMouseTo d1 500 >> wait 500 >> mouseDown >> wait 200
  moveMouseTo d2 1000 >> wait 1000
  moveMouseTo d3 1000 >> wait 1000 >> mouseUp


tutorial :: Script
tutorial = execWriter $ do
    dragOntoVia (DNA 0 NE) (DNA 2 W) (DNA 3 W)
    wait 2000
    dragOnto (DNA 2 NE) (DNA 5 W)
    wait 2000
    dragOnto (DNA 6 NE) (DNA 7 W)
    wait 2000
    dragOnto (DNA 7 NE) (DNA 8 W)
    wait 2000
    click (DNA 9 NE)
    wait 3000
    startAnim
    wait $ 10000 + 2000  -- Long enough to run animation (see videoSpeed)
