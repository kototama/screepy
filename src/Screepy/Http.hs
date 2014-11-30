{-# LANGUAGE OverloadedStrings #-}

module Screepy.Http (httpErrorToMsg) where

import qualified Network.HTTP.Client        as HC
import           Network.Wreq.Lens
import           Control.Lens
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString            as B

httpErrorToMsg :: HC.HttpException -> String
httpErrorToMsg (HC.StatusCodeException s _ _) =
  C.unpack $ B.concat [s ^. statusMessage,
                       " (error ",
                       C.pack . show $ s ^. statusCode,
                       ")"]
httpErrorToMsg e = show e
