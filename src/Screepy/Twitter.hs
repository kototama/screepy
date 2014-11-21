{-# LANGUAGE OverloadedStrings #-}

module Screepy.Twitter (TwitterConf(..),
                        getPhotos,
                        doRequest)
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
  
doRequest :: TwitterConf -> String -> Params -> IO (Response BL.ByteString)
doRequest conf segment getparams = do
  let defaultOpts = defaults
             & auth .~ (oauth2Bearer . getToken . token $ conf)
      opts = foldl (\o p  -> o & param (fst p) .~ [(snd p)]) defaultOpts getparams
  getWith opts url
    where url = (baseUrl conf) ++ segment

getPhotos :: TwitterConf -> Params -> IO [Text]
getPhotos conf reqparams = do
  r <- doRequest conf "statuses/user_timeline.json" reqparams
  return $ r ^.. responseBody
    . values
    . key "extended_entities"
    . key "media"
    . values
    . filtered (elemOf (key "type") "photo")
    . key "media_url_https" . _String
