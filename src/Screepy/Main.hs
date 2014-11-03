module Screepy.Main (main) where

import Screepy.Config (loadConfig, Config(..))
import qualified Screepy.Auth as Auth

main :: IO ()
main = do
  config <- loadConfig "screepy.yaml"
  let creds = Auth.createBearerTokenCredentials (authKey config) (authSecret config)
  putStr $ show creds
  token <- Auth.getBearerToken creds
  putStr $ show token
