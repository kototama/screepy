{-# LANGUAGE OverloadedStrings #-}

module Screepy.Main (main) where

import           Control.Monad.Error        (runErrorT)
import           Control.Monad.Trans.Resource (runResourceT, ResourceT)
import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Data.List.Split              (splitOn)
import qualified Screepy.Auth                 as Auth
import           Screepy.Config               (Config (..), loadConfig)
import qualified Screepy.Config               as CO
import           Screepy.Fetcher
import           Screepy.Photo
import           Screepy.Twitter
import qualified Screepy.Twitter              as TW

mkFilePath :: Photo -> FilePath
mkFilePath photo = last $ splitOn "/" (url photo)

sinkPhotos :: Sink Photo (ResourceT IO) ()
sinkPhotos = do
    mphoto <- await
    case mphoto of
        Nothing -> return ()
        Just photo -> do
          yield ct =$ CB.sinkFile fp
          sinkPhotos
          where fp = mkFilePath photo
                ct = BL.toStrict $ content photo

main :: IO ()
main = do
  config <- loadConfig "screepy.cfg"
  let k = C.pack (authKey config)
      s = C.pack (authSecret config)
      creds = Auth.createBearerTokenCredentials k s
      baseUrlV = (CO.baseUrl config)
  tk <- runErrorT $ Auth.getBearerToken "https://api.twitter.com/oauth2/token" creds
  case tk of
    Left err -> do
      putStr $ show err
    Right tok -> do
      let conf = TwitterConf { TW.baseUrl = baseUrlV,
                               token = tok}
      photosResp <- runErrorT $ getPhotosUrls conf [("screen_name", "nasa"), ("count", "100")]
      case photosResp of
        Right resp -> do
          -- photosResp2 <- runErrorT $ fetchAllPhotos conf [ ("screen_name", "nasa")
          --                                                 -- , ("max_id", T.pack . show . pred . oldestTweetId $ resp)
          --                                                 , ("count", "200")
          --                                                 ]
          putStrLn . show $ resp
          -- let firstPhoto = head . photosUrls $ resp
          -- resp2 <- runErrorT $ fetchPhoto firstPhoto
          -- case resp2 of
          --   Right photo -> BL.writeFile "/tmp/photo1.png" (content photo)
          --   Left _ -> putStr "error"
          resp2 <- runErrorT . fetchPhotos $ photosUrls resp
          case resp2 of
            Right photos -> runResourceT $ CL.sourceList photos $$ sinkPhotos
            Left _ -> error "error"

        Left _ -> putStr "error"
