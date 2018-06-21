module Main ( main ) where

import qualified Kaleidogen (main)
import Language.Javascript.JSaddle.WebKitGTK (run)

main = run Kaleidogen.main
