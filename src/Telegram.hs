{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Telegram (handleUpdate) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Text.Printf
import Control.Concurrent
import System.IO.Temp
import System.IO
import System.Process.Typed
import Crypto.Hash
import Data.ByteArray (convert)
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Error.Class
import Network.HTTP.Client      (newManager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Servant.Client.Internal.HttpClient (ClientM(..))
import Web.Telegram.API.Bot

import GenPNG

hashMessage :: T.Text -> BS.ByteString
hashMessage t = BS.take len s
  where
    sha :: BS.ByteString -> Digest SHA256
    sha = hash

    s = convert (sha (T.encodeUtf8 t))
    x = BS.last s
    len = fromIntegral x `mod` 20

handleUpdate :: Maybe String -> Update -> TelegramClient ()
handleUpdate helper Update{ message = Just m } = do
  let c = ChatId (chat_id (chat m))
  liftIO $ printf "message from %s: %s\n" (maybe "?" user_first_name (from m)) (maybe "" T.unpack (text m))
  if "/start" `T.isPrefixOf` fromMaybe "" (text m)
  then do
    rm <- sendMessageM $ sendMessageRequest c $
      "Hi! I am @KaleidogenBot. I will respond to every message from you with a new " <>
      "nice pattern. Note that the same message will always produce the same pattern. " <>
      "You can also go to https://kaleidogen.nomeata.de/ and breed these patterns."
    return ()
  else do
    m1 <- sendMessageM $ sendMessageRequest c "One moment…"
    withPNGFile helper (hashMessage (fromMaybe "" (text m))) $ \pngFN -> do
      m2 <- uploadPhotoM $ uploadPhotoRequest c
        (FileUpload (Just "image/png") (FileUploadFile pngFN))
      return ()
handleUpdate _ u =
  liftIO $ putStrLn $ "Unhandled message: " ++ show u

deriving instance MonadMask ClientM
