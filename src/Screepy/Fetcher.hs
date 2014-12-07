{-# LANGUAGE OverloadedStrings #-}

module Screepy.Fetcher
       ( fetchPhoto
       , FetcherError(..)) where

import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified Data.Text as T
import           Network.Wreq         (get)
import           Network.Wreq.Lens    (Response, responseBody)
import Screepy.Photo (Photo(..))
import           Control.Monad.Except
import           Control.Exception    (try)
import           Screepy.Http         (httpErrorToMsg)
import           Control.Lens

data FetcherError = HttpError String

liftReq :: IO (Response BL.ByteString) -> ExceptT FetcherError IO (Response BL.ByteString)
liftReq req = do
  r <- liftIO $ try req
  case r of
    Right x -> return x
    Left e -> throwError $ HttpError . httpErrorToMsg $ e

fetchPhoto :: Text -> ExceptT FetcherError IO Photo
fetchPhoto u = do
    r <- liftReq . get . T.unpack $ u
    return Photo { content = r ^. responseBody
                 , url = u}
