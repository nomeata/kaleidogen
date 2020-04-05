import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Hex as T
import Options.Applicative
import Control.Monad (join)
import System.IO.Temp
import System.IO
import System.Process.Typed

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
            (  long "hex"
            <> metavar "HEX"
            <> help "hex encoding of kalediogen"
            )
        <*> strOption
            (  long "out"
            <> short 'o'
            <> metavar "FILE"
            <> help "file to write"
            )

work :: Maybe String -> String -> String -> IO ()
work helper hex out = do
    bytes <- maybe (error "Not a valid hex string") return $ T.decodeHex (T.pack hex)
    pngData <- genPNG helper bytes
    LBS.writeFile out pngData
