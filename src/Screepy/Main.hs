module Screepy.Main (main) where

import Screepy.Config (loadConfig, Config(..))
import qualified Screepy.Auth as Auth

main :: IO ()
main = do
  config <- loadConfig "screepy.yaml"
  let bToken = Auth.createBearerTokenCredentials (authKey config) (authSecret config)
  putStr $ show bToken
