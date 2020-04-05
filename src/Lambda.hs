{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BSC (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Hex as T (decodeHex)
import qualified Data.ByteString.Base64.Lazy as Base64
import Control.Monad
import Aws.Lambda
import Aws.Lambda.Runtime
import GHC.Generics
import Data.Aeson
import Data.Maybe
import System.Environment
import Web.Telegram.API.Bot.API
import Network.HTTP.Client      (newManager, Manager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)

import GenPNG
import Telegram

type TC = (Token, Manager)

runTC (token, manager) = runTelegramClient token manager

getTelegramSettings :: IO TC
getTelegramSettings = do
  token <- getEnv "TELEGRAM_TOKEN"
  let t = Token ("bot" <> T.pack token)
  manager <- newManager tlsManagerSettings
  return (t, manager)

main :: IO ()
main = do
    tc <- getTelegramSettings
    runLambda (run tc)
  where
   run :: TC -> LambdaOptions -> IO (Either String LambdaResult)
   run tc opts = do
    result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
    either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result

deriving instance Show LambdaOptions

data Event = Event
  { path :: T.Text
  , body :: Maybe T.Text
  } deriving (Generic, FromJSON)

data Response = Response
    { statusCode :: Int
    , headers :: Value
    , body :: T.Text
    , isBase64Encoded :: Bool
    } deriving (Generic, ToJSON)

isImgPath :: T.Text -> Maybe T.Text
isImgPath  = T.stripPrefix "/img/" >=> T.stripSuffix ".png"

handler :: TC -> Event -> Context -> IO (Either String Response)
handler tc Event{body, path} context
    | Just bytes <- isImgPath path >>= T.decodeHex = do
        let pngData = genPurePNG bytes
        pure $ Right Response
            { statusCode = 200
            , headers = object [
                "Content-Type" .= ("image/png" :: String)
            ]
            , isBase64Encoded = True
            , body = T.decodeUtf8 $ LBS.toStrict $ Base64.encode pngData
            }

    | Just hex <- isImgPath path =
        pure $ Right Response
            { statusCode = 400
            , headers = object [
                "Content-Type" .= ("text/plain" :: String)
            ]
            , isBase64Encoded = False
            , body = "Filename not a valid hex string"
            }

    | path == "/telegram" =
        case eitherDecode (LBS.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
            Left err -> pure $ Right Response
                { statusCode = 400
                , headers = object [
                    "Content-Type" .= ("text/plain" :: String)
                ]
                , isBase64Encoded = False
                , body = "Could not decode message: " <> T.pack err
                }
            Right update -> do
                runTC tc $ handleUpdate Nothing update
                pure $ Right Response
                    { statusCode = 200
                    , headers = object [
                        "Content-Type" .= ("text/plain" :: String)
                    ]
                    , isBase64Encoded = False
                    , body = "Done"
                    }

    | otherwise =
        pure $ Right Response
            { statusCode = 200
            , headers = object [
                "Content-Type" .= ("text/plain" :: String)
            ]
            , isBase64Encoded = False
            , body = "Nothing to see here at " <> path
            }
