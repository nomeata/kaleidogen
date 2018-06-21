module Main ( main ) where

import qualified Kaleidogen (main)
import Language.Javascript.JSaddle.WKWebView (run)

main = run Kaleidogen.main
