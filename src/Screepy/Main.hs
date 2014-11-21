module Screepy.Main (main) where

import qualified Data.ByteString.Char8  as C
import qualified Screepy.Auth         as Auth
import           Screepy.Config       (Config (..), loadConfig)
import qualified Screepy.Twitter as Twitter
import Control.Monad.Except(runExceptT)
import qualified Data.Text.IO as T

main :: IO ()
main = do
  config <- loadConfig "screepy.yaml"
  let k = C.pack (authKey config)
      s = C.pack (authSecret config)
      creds = Auth.createBearerTokenCredentials k s
  tk <- runExceptT $ Auth.getBearerToken "https://api.twitter.com/oauth2/token" creds
  case tk of
    Left err -> do
      putStr $ show err
    Right token -> do
      pictures <- Twitter.getPictures token
      mapM_ T.putStrLn $ pictures
