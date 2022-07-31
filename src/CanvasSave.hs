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
import Data.Functor.Const
import Data.FileEmbed
import Language.Javascript.JSaddle.Object

import System.Directory
import System.IO
import System.IO.Error
import System.FilePath
import Control.Monad.Trans
import System.Process     (rawSystem)

-- No need to pull in lens just for this function
(^.) :: t -> ((a1 -> Const a1 b1) -> t -> Const a2 b2) -> a2
s ^. l = getConst (l Const s)


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
    liftIO $ (`catchIOError` print) $ do
        let dir = "/sdcard/Android/data/de.nomeata.kaleidogen"
        createDirectoryIfMissing True dir
        let fn = dir </> "test.txt"
        writeFile fn "hi"
        rawSystem "am"
            ["start"
            , "-a", "android.intent.action.Send"
            , "-t", "text/plain"
            , "--eu", "android.intent.extra.STREAM", "file://" ++ fn
            ]
        return ()
    return ()

{-
canvas.toBlob(function(blob) {
    saveAs(blob, "pretty image.png");
});
-}
