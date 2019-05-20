module Main where

import MainWidget
import Reflex.Dom.Internal

main :: IO ()
main = run MainWidget.mainWidget
