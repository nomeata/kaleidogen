{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

module Telegram (handleUpdate) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Printf
import Crypto.Hash
import Data.ByteArray (convert)
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad.Catch
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
    _rm1 <- sendMessageM $ sendMessageRequest c $
      "Hi! I am @KaleidogenBot. I will respond to every message from you with a new " <>
      "nice pattern. Note that the same message will always produce the same pattern. " <>
      "You can also go to https://kaleidogen.nomeata.de/ and breed these patterns. Or " <>
      "just play below, directly in Telegram (you can share that game, too!):"

    _rm2 <- sendGameM $ sendGameRequest (fromIntegral (chat_id (chat m))) "kaleidogen"

    return ()
  else do
    _m1 <- sendMessageM $ sendMessageRequest c "One moment…"
    withPNGFile helper (hashMessage (fromMaybe "" (text m))) $ \pngFN -> do
      _m2 <- uploadPhotoM $ uploadPhotoRequest c
        (FileUpload (Just "image/png") (FileUploadFile pngFN))
      return ()
handleUpdate _helper Update{ callback_query = Just q } |
  Just "kaleidogen" <- cq_game_short_name q
  = do
  liftIO $ printf "callback query from %s\n" (user_first_name (cq_from q))
  _ <- answerCallbackQueryM $
    (answerCallbackQueryRequest (cq_id q))
    { cq_url = Just "https://kaleidogen.nomeata.de/"
    }
  return ()

handleUpdate _ u =
  liftIO $ putStrLn $ "Unhandled message: " ++ show u

deriving instance MonadMask ClientM
