{-# LANGUAGE OverloadedStrings #-}

module Screepy.Twitter (getPictures)
       where

import Screepy.Auth (BearerToken, getToken)
import           Network.Wreq
import           Control.Lens
import Data.Aeson.Lens (key, _String, values)
import Data.Text(Text)

getPictures :: BearerToken -> IO [Text]
getPictures bearerToken = do
  let opts = defaults
             & auth .~ (oauth2Bearer $ getToken bearerToken)
             & param "screen_name" .~ ["nasa"]
             & param "count" .~ ["10"]
  r <- getWith opts "https://api.twitter.com/1.1/statuses/user_timeline.json"
  return $ r ^.. responseBody . values . key "extended_entities" . key "media" . values . key "media_url_https" . _String
