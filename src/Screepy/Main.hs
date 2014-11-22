{-# LANGUAGE OverloadedStrings #-}

module Screepy.Main (main) where

import           Control.Monad.Except  (runExceptT)
import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T
import qualified Screepy.Auth          as Auth
import           Screepy.Config        (Config (..), loadConfig)
import qualified Screepy.Config        as CO
import           Screepy.Twitter
import qualified Screepy.Twitter       as TW


main :: IO ()
main = do
  config <- loadConfig "screepy.yaml"
  let k = C.pack (authKey config)
      s = C.pack (authSecret config)
      creds = Auth.createBearerTokenCredentials k s
      baseUrlV = (CO.baseUrl config)
  tk <- runExceptT $ Auth.getBearerToken "https://api.twitter.com/oauth2/token" creds
  case tk of
    Left err -> do
      putStr $ show err
    Right token -> do
      let conf = TwitterConf { TW.baseUrl = baseUrlV,
                               token = token}
      photosResp <- getPhotos conf [("screen_name", "nasa"), ("count", "1")]
      photosResp2 <- getPhotos conf [("screen_name", "nasa"),
                                     ("count", "2"),
                                     ("max_id", T.pack . show . pred . oldestTweetId $ photosResp)]
      putStrLn . show . photosUrls $ photosResp
      putStrLn . show . photosUrls $ photosResp2
