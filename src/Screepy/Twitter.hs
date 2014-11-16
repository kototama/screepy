{-# LANGUAGE OverloadedStrings #-}

module Screepy.Twitter (getPictures)
       where

import Screepy.Auth (BearerToken, getToken)
import           Network.Wreq
import           Network.Wreq.Lens
import           Control.Lens
import Data.Aeson.Lens (key, nth, _String)
import Data.Maybe (fromJust)

import qualified Data.Text as T

import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy        as BL

getPictures :: BearerToken -> IO ()
getPictures bearerToken = do
  let opts = defaults
             & auth .~ (oauth2Bearer $ getToken bearerToken)
             & param "screen_name" .~ ["nasa"]
             & param "count" .~ ["2"]
  r <- getWith opts "https://api.twitter.com/1.1/statuses/user_timeline.json"
  -- BL.putStr $ r ^. responseBody
  let v = fromJust $ r ^? responseBody . nth 0 . key "extended_entities" . key "media" . nth 0 . key "media_url_https" . _String
  putStr . T.unpack $ v
  return ()
