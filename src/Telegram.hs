{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Text.Printf
import Options.Applicative
import Control.Monad (join)
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

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "General program title/description"
  <> progDesc "What does this thing do?"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> strOption
            (  long "helper"
            <> metavar "PATH"
            <> help "path to kaleidogen-gl-runner"
            )
        <*> strOption
            (  long "token"
            <> metavar "TOKEN"
            <> help "telegram token "
            )

hashMessage :: T.Text -> BS.ByteString
hashMessage t = BS.take len s
  where
    sha :: BS.ByteString -> Digest SHA256
    sha = hash

    s = convert (sha (T.encodeUtf8 t))
    x = BS.last s
    len = fromIntegral x `mod` 20


handleUpdate :: String -> Update -> TelegramClient ()
handleUpdate helper Update{ inline_query = Just q } = do
  liftIO $ putStrLn $ "inline query: " ++ show (query_query q)
  r <- answerInlineQueryM $
    (answerInlineQueryRequest (query_id q)
    [ (inlineQueryResultPhoto
        ("testid" <> query_query q)
        "https://www.joachim-breitner.de/various/free-paradoxes.jpg"
        "https://www.joachim-breitner.de/various/free-paradoxes.jpg"
      ) { iq_res_title = Just "An image!"
        , iq_res_photo_width = Just 1000
        , iq_res_photo_height = Just 1000
        }
    ]
    ){ query_cache_time = Just 0 }
  unless (result r) $
    liftIO $ putStrLn "answerInlineQuery failed"
handleUpdate helper Update{ message = Just m } = do
  liftIO $ printf "message from %s: %s\n" (maybe "?" user_first_name (from m)) (maybe "" T.unpack (text m))
  if "/start" `T.isPrefixOf` fromMaybe "" (text m)
  then do
    rm <- sendMessageM $ sendMessageRequest (ChatId (chat_id (chat m))) $
      "Hi! I am @KaleidogenBot. I will respond to every message from you with a new " <>
      "nice pattern. Note that the same message will always produce the same pattern. " <>
      "You can also go to https://kaleidogen.nomeata.de/ and breed these patterns."
    return ()
  else
    withPNGFile helper (hashMessage (fromMaybe "" (text m))) $ \pngFN -> do
      rm <- uploadPhotoM $ uploadPhotoRequest
        (ChatId (chat_id (chat m)))
        (FileUpload (Just "image/png") (FileUploadFile pngFN))
      -- liftIO $ print rm
      return ()
handleUpdate _ u =
  liftIO $ putStrLn $ "Unhandled message: " ++ show u

poll :: String -> Maybe Int -> TelegramClient ()
poll helper offset = do
  liftIO $ putStrLn "Polling"
  updates <- catchError (result <$> getUpdatesM getUpdatesRequest
    { updates_offset = offset
    , updates_allowed_updates = Just []
    , updates_limit = Just 1
    , updates_timeout = Just 10
    }) (\r -> liftIO $ do
       print r
       threadDelay (10*1000*1000)
       return []
    )
  if null updates
  then do
    liftIO $ putStrLn "No message"
    poll helper offset
  else do
    liftIO $ putStrLn "Got a message"
    let u = head updates
    catchError (handleUpdate helper u) $ \r ->
       liftIO $ print r
    poll helper (Just (update_id u + 1))

work :: String -> String -> IO ()
work helper token = do
  let t = Token ("bot" <> T.pack token)
  manager <- newManager tlsManagerSettings
  res <- runTelegramClient t manager $ poll helper Nothing
  either (putStrLn . ("failure: " ++) . show) return res


deriving instance MonadMask ClientM
