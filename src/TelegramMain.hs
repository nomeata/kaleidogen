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
import Telegram

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
        <$> (
          Just <$> strOption
            (  long "helper"
            <> metavar "PATH"
            <> help "path to kaleidogen-gl-runner"
            )
          <|>
          flag' Nothing
            (  long "pure"
            <> help "use pure Haskell"
            )
          )
        <*> strOption
            (  long "token"
            <> metavar "TOKEN"
            <> help "telegram token "
            )

poll :: Maybe String -> Maybe Int -> TelegramClient ()
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

work :: Maybe String -> String -> IO ()
work helper token = do
  let t = Token ("bot" <> T.pack token)
  manager <- newManager tlsManagerSettings
  res <- runTelegramClient t manager $ poll helper Nothing
  either (putStrLn . ("failure: " ++) . show) return res

