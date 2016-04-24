{-# LANGUAGE OverloadedStrings #-}


module Screepy.Main (main) where

import           Control.Monad.Error        (runErrorT)
import qualified Data.ByteString.Char8        as C
import qualified Screepy.Auth                 as Auth
import           Screepy.Config               (Config (..), loadConfig)
import qualified Screepy.Config               as CO
import           Screepy.Fetch
import           Screepy.Dump (dumpPhotos)
import           Screepy.Twitter
import qualified Screepy.Twitter              as TW

main :: IO ()
main = do
  config <- loadConfig "screepy.cfg"
  let k = C.pack (authKey config)
      s = C.pack (authSecret config)
      creds = Auth.createBearerTokenCredentials k s
      baseUrlV = (CO.baseUrl config)
      directory = (CO.dumpDirectory config)
  tk <- runErrorT $ Auth.getBearerToken "https://api.twitter.com/oauth2/token" creds
  case tk of
    Left err -> do
      putStr $ show err
    Right tok -> do
      let conf = TwitterConf { TW.baseUrl = baseUrlV,
                               token = tok}
      photosResp <- runErrorT $ getPhotosUrls conf [("screen_name", "nasa"), ("count", "50")]
      case photosResp of
        Right resp -> do
          putStrLn . show $ resp
          resp2 <- runErrorT . fetchPhotos $ resp
          case resp2 of
            Right photos -> dumpPhotos photos directory
            Left _ -> error "error"
        Left _ -> putStr "error"
