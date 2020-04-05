{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text as T
import qualified Text.Hex as T (decodeHex)
import qualified Data.ByteString.Base64.Lazy as Base64
import Control.Monad

import GenPNG

import Aws.Lambda
import Aws.Lambda.Runtime
import GHC.Generics
import Data.Aeson
import System.Directory

main :: IO ()
main = runLambda run
  where
   run :: LambdaOptions -> IO (Either String LambdaResult)
   run opts = do
    result <- handler (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
    either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result

deriving instance Show LambdaOptions

newtype Event = Event
  { path :: T.Text
  } deriving (Generic, FromJSON)

data Response = Response
    { statusCode :: Int
    , headers :: Value
    , body :: String
    , isBase64Encoded :: Bool
    } deriving (Generic, ToJSON)

html :: String
html = "<h1>Hello World</h1>"

isImgPath :: T.Text -> Maybe T.Text
isImgPath  = T.stripPrefix "/img/" >=> T.stripSuffix ".png"

handler :: Event -> Context -> IO (Either String Response)
handler event context =
    case isImgPath (path event) of
        Just hex | Just bytes <- T.decodeHex hex -> do
            let pngData = genPurePNG bytes
            pure $ Right Response
                { statusCode = 200
                , headers = object [
                    "Content-Type" .= ("image/png" :: String)
                ]
                , isBase64Encoded = True
                , body = BSC.unpack $ Base64.encode pngData
                }
        Just hex ->
            pure $ Right Response
                { statusCode = 400
                , headers = object [
                    "Content-Type" .= ("text/plain" :: String)
                ]
                , isBase64Encoded = False
                , body = "Filename not a valid hex string"
                }
        Nothing ->
            pure $ Right Response
                { statusCode = 200
                , headers = object [
                    "Content-Type" .= ("text/plain" :: String)
                ]
                , isBase64Encoded = False
                , body = "Nothing to see here at " <> T.unpack (path event)
                }
