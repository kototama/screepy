{-# LANGUAGE OverloadedStrings #-}

-- |
-- High-level queries for the user timeline.
module Screepy.Twitter
       (  TwitterConf(..)
        , PhotosResp(..)
        , getPhotosUrls
        , fetchAllPhotosUrls
        , Params
        , TwitterError(NoTweet, HttpError)) where

import           Control.Exception    (try)
import           Control.Lens
import           Control.Monad.Error
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
import           Data.Time.Clock      ()
import           Data.Time.Format
import           System.Locale
import           Data.Time

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
             , photosUrls    :: [String]
               -- ^ URLs of the photos
             , creationTimes :: [UTCTime]
               -- ^ creation time
             } deriving Show

-- | Represent errors than can occurs during a call.
data TwitterError = NoTweet | NoMoreTweet PhotosResp | HttpError String deriving Show

instance Error TwitterError where
  noMsg    = HttpError "Twitter error during fetch!"
  strMsg s = HttpError s

doGetReq :: TwitterConf -> String -> Params -> IO (Response BL.ByteString)
doGetReq conf segment getparams = do
  let defaultOpts = defaults
  -- TODO see how to get rid of the Just constructor here
             & auth .~ (Just (oauth2Bearer . getToken . token $ conf))
      opts = foldl (\o p  -> o & param (fst p) .~ [snd p]) defaultOpts getparams
      url = baseUrl conf ++ segment
  getWith opts url

liftReq :: IO (Response BL.ByteString) -> ErrorT TwitterError IO (Response BL.ByteString)
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
getPhotosUrls :: TwitterConf -> Params -> ErrorT TwitterError IO PhotosResp
getPhotosUrls conf reqparams = do
  r <- liftReq $ doGetReq conf "statuses/user_timeline.json" reqparams
  let times = r ^.. responseBody . values . key "created_at" . _String
  liftIO . putStr . show $ times
  let ids = r ^.. responseBody . values . key "id" . _Integer
  if null ids
    then throwError NoTweet
    else let newestId = head ids
             oldestId = last ids
             urls = r ^.. responseBody
                       . values
                       . key "extended_entities"
                       . key "media"
                       . values
                       . filtered (elemOf (key "type"._String) "photo")
                       . key "media_url_https" . _String
             timesStrings = r ^.. responseBody
                            . values
                            . key "created_at" . _String in
         return PhotosResp { newestTweetId = newestId
                           , oldestTweetId = oldestId
                           , photosUrls = map T.unpack urls
                           , creationTimes = map (parseCreationTime . T.unpack) timesStrings
                           }


parseCreationTime :: String -> UTCTime
parseCreationTime dateString =
  readTime defaultTimeLocale "%a %b %d %T %z %Y" dateString :: UTCTime


fetchAllPhotosUrls' :: TwitterConf -> Params -> PhotosResp -> ErrorT TwitterError IO PhotosResp
fetchAllPhotosUrls' conf params accumulator = do
    let maxId = T.pack . show . pred . oldestTweetId $ accumulator
        reqparams = unionBy ((==) `on` fst) [("max_id", maxId)] params
    resp <- getPhotosUrls conf reqparams
            `catchError`
            (\e -> case e of
                NoTweet -> throwError $ NoMoreTweet accumulator
                _ -> throwError e)

    fetchAllPhotosUrls' conf params PhotosResp { newestTweetId = newestTweetId accumulator
                                               , oldestTweetId = oldestTweetId resp
                                               , photosUrls =  photosUrls accumulator ++ photosUrls resp
                                               , creationTimes =  creationTimes accumulator ++ creationTimes resp
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
fetchAllPhotosUrls :: TwitterConf -> Params -> ErrorT TwitterError IO PhotosResp
fetchAllPhotosUrls conf reqparams = do
  resp <- getPhotosUrls conf reqparams
  fetchAllPhotosUrls' conf reqparams resp
    `catchError`
    (\e -> case e of
        NoMoreTweet actual -> return actual
        _ -> throwError e)
