module CanvasSave where

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import GHCJS.DOM.HTMLScriptElement
import GHCJS.DOM.Types hiding (Text)

import qualified Data.Text as T
import Language.Javascript.JSaddle (toJSVal)
import Language.Javascript.JSaddle.Object
import Control.Lens ((^.))

src :: T.Text
src = T.pack "https://fastcdn.org/FileSaver.js/1.1.20151003/FileSaver.min.js"

register :: MonadJSM m => m ()
register = do
    doc <- currentDocumentUnchecked
    headEl <- getHeadUnchecked doc
    script <- uncheckedCastTo HTMLScriptElement <$> createElement doc "script"
    setSrc script src
    appendChild_ headEl script

save :: MonadJSM m => T.Text -> HTMLCanvasElement -> m ()
save name e = liftJSM $ do
    domEl <- toJSVal e
    _ <- domEl ^. js1 "toBlob" (fun $ \_ _ [blob] -> () <$ jsg2 "saveAs" blob name )
    return ()

{-
canvas.toBlob(function(blob) {
    saveAs(blob, "pretty image.png");
});
-}
