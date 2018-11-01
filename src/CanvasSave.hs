module CanvasSave where

import qualified Data.Text as T
import Language.Javascript.JSaddle (toJSVal, liftJSM, MonadJSM)
import Language.Javascript.JSaddle.Object
import Control.Lens ((^.))
import Reflex.Dom

header :: DomBuilder t m => m ()
header = do
    elAttr (T.pack "script") (T.pack "src" =: T.pack "https://fastcdn.org/FileSaver.js/1.1.20151003/FileSaver.min.js") (return ())

save :: MonadJSM m => T.Text -> El t -> m ()
save name e = liftJSM $ do
    domEl <- toJSVal (_element_raw e)
    _ <- domEl ^. js1 "toBlob" (fun $ \_ _ [blob] -> () <$ jsg2 "saveAs" blob name )
    return ()

{-
canvas.toBlob(function(blob) {
    saveAs(blob, "pretty image.png");
});
-}
