module GenPNG where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as T
import System.IO.Temp
import System.IO
import System.Process.Typed
import Codec.Picture (encodePng)

import Expression
import GLSL
import Shaders
import Img

genPurePNG :: BS.ByteString -> LBS.ByteString
genPurePNG bytes = encodePng $ img2Juicy 1024 $ toImg rna
  where
    dna = BS.unpack bytes
    rna = dna2rna dna

genPNG :: Maybe String -> BS.ByteString -> IO LBS.ByteString
genPNG helper bytes = case helper of
    Just h -> do
        let vert = circularVertexShader
        let frag = toFragmentShader rna

        withSystemTempFile "kaleidogen.vert" $ \vertexFile vertexHandle ->
          withSystemTempFile "kaleidogen.frag" $ \fragFile fragHandle -> do
            T.hPutStr vertexHandle vert
            hClose vertexHandle

            T.hPutStr fragHandle frag
            hClose fragHandle

            (pngData, _err) <- readProcess_ (proc h ["512","512",vertexFile,fragFile])

            return pngData
    Nothing -> pure $ encodePng $ img2Juicy 1024 $ toImg rna
  where
    dna = BS.unpack bytes
    rna = dna2rna dna

withPNGFile :: (MonadIO m, MonadMask m) => Maybe String -> BS.ByteString -> (FilePath -> m a) -> m a
withPNGFile helper bytes k =
    withSystemTempFile "kaleidogen.png" $ \f h -> do
        liftIO $ do
            genPNG helper bytes >>= LBS.hPutStr h
            hClose h
        k f
