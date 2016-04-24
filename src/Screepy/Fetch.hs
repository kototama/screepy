{-# LANGUAGE OverloadedStrings #-}

module Screepy.Fetch
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
import Screepy.Twitter (PhotosResp(..))
import           Data.Time (UTCTime)

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

fetchPhoto :: String -> UTCTime -> ErrorT FetcherError IO Photo
fetchPhoto u ctime = do
    r <- liftReq . get $ u
    return Photo { content = r ^. responseBody
                 , url = u
                 , creationTime = ctime }

fetchPhotos :: PhotosResp -> ErrorT FetcherError IO [Photo]
fetchPhotos resp = mapM (uncurry fetchPhoto) $ zip urls ctimes
  where urls = photosUrls resp
        ctimes = creationTimes resp
