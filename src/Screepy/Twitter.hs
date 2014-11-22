{-# LANGUAGE OverloadedStrings #-}

module Screepy.Twitter (TwitterConf(..),
                        getPhotos,
                        doGetReq)
       where

import Screepy.Auth (BearerToken, getToken)
import           Network.Wreq
import           Control.Lens
import Data.Aeson.Lens (key, _String, values)
import Data.Text(Text)
import qualified Data.ByteString.Lazy       as BL
type Params = [(Text,Text)]

data TwitterConf = TwitterConf { token :: BearerToken,
                                 baseUrl :: String
                               } deriving Show

doGetReq :: TwitterConf -> String -> Params -> IO (Response BL.ByteString)
doGetReq conf segment getparams = do
  let defaultOpts = defaults
             & auth .~ (oauth2Bearer . getToken . token $ conf)
      opts = foldl (\o p  -> o & param (fst p) .~ [snd p]) defaultOpts getparams
      url = (baseUrl conf) ++ segment
  getWith opts url

getPhotos :: TwitterConf -> Params -> IO [Text]
getPhotos conf reqparams = do
  r <- doGetReq conf "statuses/user_timeline.json" reqparams
  return $ r ^.. responseBody
    . values
    . key "extended_entities"
    . key "media"
    . values
    . filtered (elemOf (key "type"._String) "photo")
    . key "media_url_https" . _String
