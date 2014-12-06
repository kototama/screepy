{-# LANGUAGE OverloadedStrings #-}

-- |
-- Interaction with the timeline.
module Screepy.Twitter
       (  TwitterConf(..)
        , PhotosResp(..)
        , getPhotos
        , getMaximumOfPhotos
        , Params
        , TwitterError(..)) where

import           Control.Exception   (try)
import           Control.Monad.Except
import           Control.Lens
import           Data.Aeson.Lens      (key, values, _Integer, _String)
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified Data.Text as T
import           Network.Wreq (getWith, defaults, auth, oauth2Bearer, param, Response)
import           Network.Wreq.Lens (responseBody)
import           Screepy.Auth         (BearerToken, getToken)
import Screepy.Http (httpErrorToMsg)
import Data.List (union)

-- | A list of parameter to pass to the Twitter API calls
type Params = [(Text,Text)]

-- | Configuration passed to the functions of this module. Necessary
-- to authenticate with the Twitter API.
data TwitterConf = TwitterConf { token   :: BearerToken
                               , baseUrl :: String
                               } deriving Show

-- | Store the Photos URLs contained in a timeline
data PhotosResp =
  PhotosResp { newestTweetId :: Integer
               -- ^ id of the newest requested Tweet (containing a photo or not)
             , oldestTweetId :: Integer
               -- ^ id of the oldest request Tweet (containing a photo or not)
             , photosUrls    :: [Text]
               -- ^ URLs of the photos
             } deriving Show

-- | Represent errors than can occurs during a call
data TwitterError = NoTweet | NoMoreTweet PhotosResp | HttpError String deriving Show

doGetReq :: TwitterConf -> String -> Params -> IO (Response BL.ByteString)
doGetReq conf segment getparams = do
  let defaultOpts = defaults
             & auth .~ (oauth2Bearer . getToken . token $ conf)
      opts = foldl (\o p  -> o & param (fst p) .~ [snd p]) defaultOpts getparams
      url = (baseUrl conf) ++ segment
  getWith opts url

liftReq :: IO (Response BL.ByteString) -> ExceptT TwitterError IO (Response BL.ByteString)
liftReq req = do
  r <- liftIO $ try req
  case r of
    Right x -> return x
    Left e -> throwError $ HttpError . httpErrorToMsg $ e

-- | Filter the photos URLs in the timeline, respecting the query parameters
getPhotos :: TwitterConf -> Params -> ExceptT TwitterError IO PhotosResp
getPhotos conf reqparams = do
  r <- liftReq $ doGetReq conf "statuses/user_timeline.json" reqparams
  let ids = r ^.. responseBody . values . key "id" . _Integer
  if null ids
    then throwError NoTweet
    else do let newestId = head ids
                oldestId = last ids
                urls = r ^.. responseBody
                       . values
                       . key "extended_entities"
                       . key "media"
                       . values
                       . filtered (elemOf (key "type"._String) "photo")
                       . key "media_url_https" . _String in
              return $ PhotosResp { newestTweetId = newestId
                                  , oldestTweetId = oldestId
                                  , photosUrls = urls
                                  }

getMaximumOfPhotos' :: TwitterConf -> Params -> PhotosResp -> PhotosResp -> ExceptT TwitterError IO PhotosResp
getMaximumOfPhotos' conf params prevResp accumulator = do
    let maxId = T.pack . show . pred . oldestTweetId $ prevResp
        reqparams = [("max_id", maxId)] `union` params
    resp <- getPhotos conf reqparams
            `catchError`
            (\e -> case e of
                NoTweet -> throwError $ NoMoreTweet accumulator
                _ -> throwError e)

    getMaximumOfPhotos' conf params resp PhotosResp { newestTweetId = newestTweetId accumulator
                                                    , oldestTweetId = oldestTweetId resp
                                                    , photosUrls =  (photosUrls accumulator) ++ (photosUrls resp)
                                                    }
      
-- | Attempt to fetch the maximum number of photos URLs in the timeline                   
getMaximumOfPhotos :: TwitterConf -> Params -> ExceptT TwitterError IO PhotosResp
getMaximumOfPhotos conf reqparams = do
  resp <- getPhotos conf reqparams
  getMaximumOfPhotos' conf reqparams resp resp
    `catchError`
    (\e -> case e of
        NoMoreTweet actual -> return actual
        _ -> throwError e)
