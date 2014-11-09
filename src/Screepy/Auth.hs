{-# LANGUAGE OverloadedStrings #-}

module Screepy.Auth (createBearerTokenCredentials,
                     getBearerToken,
                     BearerToken(..))
       where

import           Control.Lens
import           Data.Aeson             (Result (Success), fromJSON)
import qualified Data.Aeson.Lens        as L
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as C
import qualified Network.HTTP.Base      as Http
import           Network.Wreq
import           Network.Wreq.Lens
import Control.Exception as E
import qualified Network.HTTP.Client as HC

newtype BearerTokenCredentials =
  BearerTokenCredentials { getCredentials :: B.ByteString } deriving Show

newtype BearerToken = BearerToken { getToken :: B.ByteString } deriving Show

createBearerTokenCredentials :: B.ByteString -> B.ByteString -> BearerTokenCredentials
createBearerTokenCredentials key secret =
  let k = C.pack $ Http.urlEncode (C.unpack key)
      s = C.pack $ Http.urlEncode (C.unpack secret)
   in
   BearerTokenCredentials $ B64.encode (B.concat [k, ":", s])

getBearerToken :: String -> BearerTokenCredentials -> IO (Either B.ByteString BearerToken)
getBearerToken url bearerTokenCredentials = do
  let creds = getCredentials bearerTokenCredentials
      authContent = B.concat ["Basic ", creds]
      opts = defaults & header "Authorization" .~ [authContent]
  a <- E.try (postWith opts url ["grant_type" := ("client_credentials" :: B.ByteString)])
  case a of
    Left (HC.StatusCodeException s _ _) -> return . Left $ msg
      where msg = B.concat [s ^. statusMessage,
                            " (error ",
                            C.pack . show $ s ^. statusCode,
                            ")"]
    Left e -> return . Left . C.pack $ show e
    Right r ->
      case r ^? responseBody . L.key "access_token" of
        Just tokenV -> case (fromJSON tokenV :: Result String) of
          Success token -> return $ Right . BearerToken . C.pack $ token
          _ -> return . Left $ "Invalid response content"
        _ -> return . Left $ "Invalid response content"
