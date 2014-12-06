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
    Right tok -> do
      let conf = TwitterConf { TW.baseUrl = baseUrlV,
                               token = tok}
      photosResp <- runExceptT $ getPhotos conf [("screen_name", "nasa"), ("count", "10")]
      case photosResp of
        Right resp -> do
          -- photosResp2 <- runExceptT $ fetchAllPhotos conf [ ("screen_name", "nasa")
          --                                                 -- , ("max_id", T.pack . show . pred . oldestTweetId $ resp)
          --                                                 , ("count", "200")
          --                                                 ]
          putStrLn . show $ resp
          -- photosResp2 <- runExceptT $ getPhotos conf [("screen_name", "nasa"),
          --                                             ("count", "50"),
          --                                             ("max_id", T.pack . show . pred . oldestTweetId $ resp)]
        Left _ -> putStr "error"
