module Screepy.Main (main) where

import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString        as B
import qualified Screepy.Auth         as Auth
import           Screepy.Config       (Config (..), loadConfig)

main :: IO ()
main = do
  config <- loadConfig "screepy.yaml"
  let k = C.pack (authKey config)
      s = C.pack (authSecret config)
      creds = Auth.createBearerTokenCredentials k s
  token <- Auth.getBearerToken "https://api.twitter.com/oauth2/token" creds
  case token of
    Left err -> do
      B.putStr err
    Right val -> B.putStr $ Auth.getToken val
