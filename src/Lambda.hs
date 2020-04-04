import qualified Data.Aeson as Aeson
import AWSLambda

main = lambdaMain handler

handler :: Aeson.Value -> IO [Int]
handler evt = do
  putStrLn "This should go to logs"
  print evt
  pure [1, 2, 3]
