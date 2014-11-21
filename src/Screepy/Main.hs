{-# LANGUAGE OverloadedStrings #-}

module Screepy.Main (main) where

import qualified Data.ByteString.Char8  as C
import qualified Screepy.Auth         as Auth
import           Screepy.Config       (Config (..), loadConfig)
import qualified Screepy.Twitter as TW
import Control.Monad.Except(runExceptT)
import qualified Data.Text.IO as T


main :: IO ()
main = do
  config <- loadConfig "screepy.yaml"
  let k = C.pack (authKey config)
      s = C.pack (authSecret config)
      creds = Auth.createBearerTokenCredentials k s
      baseUrlV = (baseUrl config)
  tk <- runExceptT $ Auth.getBearerToken "https://api.twitter.com/oauth2/token" creds
  case tk of
    Left err -> do
      putStr $ show err
    Right token -> do
      let conf = TW.TwitterConf { TW.baseUrl = baseUrlV,
                                  TW.token = token}
      pictures <- TW.getPhotos conf [("screen_name", "nasa"), ("count", "10")]
      mapM_ T.putStrLn $ pictures
