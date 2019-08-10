{-# LANGUAGE CPP #-}
module RunWidget where

import GHCJS.DOM.Types (JSM)

#if defined(MIN_VERSION_reflex_dom)
import qualified Reflex.Dom.Internal
runWidget :: JSM () -> IO ()
runWidget = Reflex.Dom.Internal.run
#elif defined(ghcjs_HOST_OS)
runWidget :: a -> a
runWidget = id
#elif defined(MIN_VERSION_jsaddle_wkwebview)
import Language.Javascript.JSaddle.WKWebView (run)
runWidget :: JSM () -> IO ()
runWidget = Language.Javascript.JSaddle.WKWebView.run
#elif defined(MIN_VERSION_jsaddle_warp)
import qualified Language.Javascript.JSaddle.Warp (run)
runWidget :: JSM () -> IO ()
runWidget = Language.Javascript.JSaddle.Warp.run 3003
#else
#error "Unsupported"
#endif
