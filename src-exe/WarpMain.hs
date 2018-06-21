module Main ( main ) where

import qualified Kaleidogen (main)
import Language.Javascript.JSaddle.Warp (run)

main = run 3709 Kaleidogen.main
