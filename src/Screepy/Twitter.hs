{-# LANGUAGE OverloadedStrings #-}

-- |
-- High-level queries for the user timeline.
module Screepy.Twitter
       (  TwitterConf(..)
        , PhotosResp(..)
        , getPhotos
        , fetchAllPhotos
        , Params
        , TwitterError(NoTweet, HttpError)) where

import           Control.Exception    (try)
import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson.Lens      (key, values, _Integer, _String)
import qualified Data.ByteString.Lazy as BL
import           Data.Function        (on)
import           Data.List            (unionBy)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.Wreq         (Response, auth, defaults, getWith,
                                       oauth2Bearer, param)
import           Network.Wreq.Lens    (responseBody)
import           Screepy.Auth         (BearerToken, getToken)
import           Screepy.Http         (httpErrorToMsg)

-- | A list of parameter to pass to the Twitter API calls.
-- 
-- Parameters are describe at <https://dev.twitter.com/rest/reference/get/statuses/user_timeline>
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
               -- ^ id of the oldest requested Tweet (containing a photo or not)
             , photosUrls    :: [Text]
               -- ^ URLs of the photos
             } deriving Show

-- | Represent errors than can occurs during a call.
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

-- | Get the photos URLs in the timeline.
-- 
-- Example:
--    
-- @
-- getPhotos conf [(\"screen_name\", \"nasa\"), (\"count\", \"10\")]
-- @
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

fetchAllPhotos' :: TwitterConf -> Params -> PhotosResp -> ExceptT TwitterError IO PhotosResp
fetchAllPhotos' conf params accumulator = do
    let maxId = T.pack . show . pred . oldestTweetId $ accumulator
        reqparams = unionBy ((==) `on` fst) [("max_id", maxId)] params
    resp <- getPhotos conf reqparams
            `catchError`
            (\e -> case e of
                NoTweet -> throwError $ NoMoreTweet accumulator
                _ -> throwError e)

    fetchAllPhotos' conf params PhotosResp { newestTweetId = newestTweetId accumulator
                                           , oldestTweetId = oldestTweetId resp
                                           , photosUrls =  (photosUrls accumulator) ++ (photosUrls resp)
                                           }

-- | Attempt to fetch the maximum number of photos URLs in the
-- timeline. Due to the constraints of the Twitter API a maximum
-- number of 3200 most-recent Tweets can be explored. A rate of 300
-- request per 15-min window is applied.
-- 
-- Example:
-- 
-- @
-- fetchAllPhotos conf [(\"screen_name\", \"nasa\"), (\"count\", \"200\")]
-- @
fetchAllPhotos :: TwitterConf -> Params -> ExceptT TwitterError IO PhotosResp
fetchAllPhotos conf reqparams = do
  resp <- getPhotos conf reqparams
  fetchAllPhotos' conf reqparams resp
    `catchError`
    (\e -> case e of
        NoMoreTweet actual -> return actual
        _ -> throwError e)
