module GenPNG where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO.Temp
import System.IO
import System.Process.Typed

import DNA
import Expression
import GLSL
import Shaders
import Img

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

            (pngData, err) <- readProcess_ (proc h ["1000","1000",vertexFile,fragFile])

            return pngData
    Nothing -> pure $ img2Png (toImg rna)
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
