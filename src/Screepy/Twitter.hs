{-# LANGUAGE OverloadedStrings #-}

module Screepy.Twitter
       (  TwitterConf(..)
        , PhotosResp(..)
        , getPhotos
        , doGetReq) where

import           Control.Lens
import           Data.Aeson.Lens      (key, nth, values, _Integer, _String)
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import           Network.Wreq
import           Screepy.Auth         (BearerToken, getToken)
type Params = [(Text,Text)]

data TwitterConf = TwitterConf { token   :: BearerToken
                               , baseUrl :: String
                               } deriving Show

data PhotosResp =
  PhotosResp { newestTweetId :: Integer
               -- ^ the id of the newest requested Tweet (containing a photo or not)
             , oldestTweetId :: Integer
               -- ^ the id of the oldest request Tweet (containing a photo or not)
             , photosUrls    :: [Text]
               -- ^ the URLs of the photos
             } deriving Show

doGetReq :: TwitterConf -> String -> Params -> IO (Response BL.ByteString)
doGetReq conf segment getparams = do
  let defaultOpts = defaults
             & auth .~ (oauth2Bearer . getToken . token $ conf)
      opts = foldl (\o p  -> o & param (fst p) .~ [snd p]) defaultOpts getparams
      url = (baseUrl conf) ++ segment
  getWith opts url

getPhotos :: TwitterConf -> Params -> IO PhotosResp
getPhotos conf reqparams = do
  r <- doGetReq conf "statuses/user_timeline.json" reqparams

  let ids = r ^.. responseBody . values . key "id" . _Integer
      newestId = head ids -- what if there are no tweets at all?
      oldestId = last ids
      urls = r ^.. responseBody
               . values
               . key "extended_entities"
               . key "media"
               . values
               . filtered (elemOf (key "type"._String) "photo")
               . key "media_url_https" . _String

  return $ PhotosResp { newestTweetId = newestId
                      , oldestTweetId = oldestId
                      , photosUrls = urls
                      }
