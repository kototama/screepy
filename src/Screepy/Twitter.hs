{-# LANGUAGE OverloadedStrings #-}

module Screepy.Twitter
       (  TwitterConf(..)
        , PhotosResp(..)
        , getPhotos
        , doGetReq
        , getMaximumOfPhotos) where

import           Control.Exception   (try)
import           Control.Monad.Except
import           Control.Lens
import           Data.Aeson.Lens      (key, nth, values, _Integer, _String)
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified Data.Text as T
import           Network.Wreq
import           Screepy.Auth         (BearerToken, getToken)
import Screepy.Http (httpErrorToMsg)

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

data TwitterError = NoTweet | NoMoreTweet [PhotosResp] | HttpError String deriving Show

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

getMaximumOfPhotos' :: TwitterConf -> PhotosResp -> [PhotosResp] -> ExceptT TwitterError IO [PhotosResp]
getMaximumOfPhotos' conf prevResp resps = do
    let maxId = T.pack . show . pred . oldestTweetId $ prevResp
        reqparams = [("screen_name", "nasa"),
                     ("max_id", maxId),
                     ("count", "200")]
    resp <- getPhotos conf reqparams `catchError` (\e -> case e of
                                                      NoTweet -> do                                                      
                                                         liftIO . putStrLn $ "no tweet"
                                                         throwError $ NoMoreTweet resps
                                                      _ -> throwError e)

    liftIO . putStrLn . show . photosUrls $ resp
    getMaximumOfPhotos' conf resp (resp : resps)
                   
getMaximumOfPhotos :: TwitterConf -> Params -> ExceptT TwitterError IO [PhotosResp]
getMaximumOfPhotos conf reqparams = do
  resp <- getPhotos conf reqparams
  getMaximumOfPhotos' conf resp [resp] `catchError` (\e -> case e of
                                                        NoMoreTweet actuals ->
                                                          do
                                                            liftIO . putStrLn $ "no more tweet"
                                                            return actuals
                                                        _ -> throwError e)
