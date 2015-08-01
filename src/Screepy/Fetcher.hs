{-# LANGUAGE OverloadedStrings #-}

module Screepy.Fetcher
       ( fetchPhoto
       , fetchPhotos
       , FetcherError(..)) where

import qualified Data.ByteString.Lazy as BL
import           Network.Wreq         (get)
import           Network.Wreq.Lens    (Response, responseBody)
import Screepy.Photo (Photo(..))
import           Control.Monad.Error
import           Control.Exception    (try)
import           Screepy.Http         (httpErrorToMsg)
import           Control.Lens

data FetcherError = HttpError String

instance Error FetcherError where
  noMsg    = HttpError "HTTP error during fetch!"
  strMsg s = HttpError s

liftReq :: IO (Response BL.ByteString) -> ErrorT FetcherError IO (Response BL.ByteString)
liftReq req = do
  r <- liftIO $ try req
  case r of
    Right x -> return x
    Left e -> throwError $ HttpError . httpErrorToMsg $ e

fetchPhoto :: String -> ErrorT FetcherError IO Photo
fetchPhoto u = do
    r <- liftReq . get $ u
    return Photo { content = r ^. responseBody
                 , url = u}

fetchPhotos :: [String] -> ErrorT FetcherError IO [Photo]
fetchPhotos urls = mapM fetchPhoto urls
