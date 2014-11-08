{-# LANGUAGE OverloadedStrings #-}

module Screepy.Auth (createBearerTokenCredentials,
                     getBearerToken
                    ) where

import           Control.Lens
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Char8  as C
import qualified Network.HTTP.Base           as Http
import           Network.Wreq
import           Network.Wreq.Lens

newtype BearerTokenCredentials =
  BearerTokenCredentials { getCredentials :: B.ByteString } deriving Show

createBearerTokenCredentials :: B.ByteString -> B.ByteString -> BearerTokenCredentials
createBearerTokenCredentials key secret =
  let k = C.pack $ Http.urlEncode (C.unpack key)
      s = C.pack $ Http.urlEncode (C.unpack secret)
   in
   BearerTokenCredentials $ B64.encode (B.concat [k, ":", s])

getBearerToken :: String -> BearerTokenCredentials -> IO String
getBearerToken url bearerTokenCredentials = do
  let creds = getCredentials bearerTokenCredentials
      authContent = B.concat ["Basic ", creds]
      opts = defaults
             & header "Authorization" .~ [authContent]
             & header "Content-Type" .~ ["application/x-www-form-urlencoded;charset=UTF-8"]
  putStr $ "creds ="
  B.putStr creds
  putStr $ show opts
  putStr "\n\n"
  r <- postWith opts url ["grant_type" := ("client_credentials" :: B.ByteString)]
  case r ^? responseBody of
    Just body -> B.putStr (BL.toStrict body)
    _ -> B.putStr "no body"
  return "win"
