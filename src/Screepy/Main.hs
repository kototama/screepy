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
      photosResp <- runExceptT $ getPhotos conf [("screen_name", "nasa"), ("count", "1")]
      case photosResp of
        Right resp -> do
          photosResp2 <- runExceptT $ fetchAllPhotos conf [ ("screen_name", "nasa")
                                                          -- , ("max_id", T.pack . show . pred . oldestTweetId $ resp)
                                                          , ("count", "200")
                                                          ]

          -- photosResp2 <- runExceptT $ getPhotos conf [("screen_name", "nasa"),
          --                                             ("count", "50"),
          --                                             ("max_id", T.pack . show . pred . oldestTweetId $ resp)]
          case photosResp2 of
               Right resp2 -> do
                 putStrLn . show . length . photosUrls $ resp2
                 let diff = (newestTweetId resp2) - (oldestTweetId resp2)
                 putStrLn $ "diff =" ++  (show diff) 
               Left _ -> putStr "error"
        Left _ -> putStr "error"
