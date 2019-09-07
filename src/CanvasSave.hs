{-# LANGUAGE TemplateHaskell #-}
module CanvasSave where

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Types hiding (Text)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import Data.FileEmbed
import Language.Javascript.JSaddle (toJSVal)
import Language.Javascript.JSaddle.Object
import Control.Lens ((^.))

import System.Directory
import System.IO
import System.IO.Error
import Control.Monad.Trans

src :: BS.ByteString
src = $(embedFile "vendor/FileSaver.1.3.8.min.js")

register :: MonadJSM m => m ()
register = do
    doc <- currentDocumentUnchecked
    headEl <- getHeadUnchecked doc
    script <- uncheckedCastTo HTMLElement <$> createElement doc "script"
    setInnerText script (T.decodeUtf8 src)
    appendChild_ headEl script

save :: MonadJSM m => T.Text -> HTMLCanvasElement -> m ()
save name e = liftJSM $ do
    domEl <- toJSVal e
    _ <- domEl ^. js1 "toBlob" (fun $ \_ _ [blob] -> () <$ jsg2 "saveAs" blob name )
    liftIO $ (`catchIOError` print) $
        createDirectory "/sdcard/Kaleidogen"
        writeFile "/sdcard/Kaleidogen/test.txt" "hi"
    return ()

{-
canvas.toBlob(function(blob) {
    saveAs(blob, "pretty image.png");
});
-}
