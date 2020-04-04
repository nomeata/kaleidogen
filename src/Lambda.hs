{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

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
    print opts
    result <- handler (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
    either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result

deriving instance Show LambdaOptions

newtype Event = Event
  { rawPath :: String
  } deriving (Generic, FromJSON)

data Response = Response
    { statusCode :: Int
    , headers :: Value
    , body :: String
    } deriving (Generic, ToJSON)

html :: String
html = "<h1>Hello World</h1>"

handler :: Event -> Context -> IO (Either String Response)
handler event context = do
    files <- getDirectoryContents "/dev"
    pure $ Right Response
        { statusCode = 200
        , headers = object [
            "Content-Type" .= ("text/html" :: String)
        ]
        , body = "<h1>Hello World</h1>" <>
          "<ol>" <> concat [ "<li>" <> f <> "</li>" | f <- files ] <> "</ol>"
        }
