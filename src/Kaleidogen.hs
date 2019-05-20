{-# LANGUAGE CPP #-}
module Kaleidogen where

import GHCJS.DOM.Types (JSM)
import MainWidget

#if defined(ghcjs_HOST_OS)
run :: a -> a
run = id
#elif defined(MIN_VERSION_jsaddle_wkwebview)
import Language.Javascript.JSaddle.WKWebView (run)
#elif defined(MIN_VERSION_jsaddle_warp)
import qualified Language.Javascript.JSaddle.Warp (run)
run :: JSM () -> IO ()
run = Language.Javascript.JSaddle.Warp.run 3003
#else
#error "Unsupported"
#endif

main :: IO ()
main = run mainWidget
