{-# LANGUAGE OverloadedStrings #-}

module Screepy.Auth ( createBearerTokenCredentials
                    , getBearerToken
                    , BearerToken(..)
                    , AuthError(..)) where

import           Control.Exception          as E
import           Control.Lens
import           Control.Monad.Error
import           Data.Aeson                 (fromJSON, Result(..))
import qualified Data.Aeson.Lens            as L
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as BL
import           Data.Either.Utils          (maybeToEither)
import qualified Network.HTTP.Base          as Http
import           Network.Wreq
import Screepy.Http (httpErrorToMsg)

newtype BearerTokenCredentials =
  BearerTokenCredentials { getCredentials :: B.ByteString } deriving Show

newtype BearerToken = BearerToken { getToken :: B.ByteString } deriving Show

data AuthError = HttpError String | InvalidCredentials | InvalidJSON String deriving Show

instance Error AuthError where
  noMsg    = HttpError "Authentification error!"
  strMsg s = HttpError s

createBearerTokenCredentials :: B.ByteString -> B.ByteString -> BearerTokenCredentials
createBearerTokenCredentials key secret =
  let k = C.pack $ Http.urlEncode (C.unpack key)
      s = C.pack $ Http.urlEncode (C.unpack secret)
   in
   BearerTokenCredentials $ B64.encode (B.concat [k, ":", s])

liftReq :: IO (Response BL.ByteString) -> ErrorT AuthError IO (Response BL.ByteString)
liftReq req = do
  r <- liftIO $ E.try req
  case r of
    Right x -> return x
    Left e -> throwError $ HttpError . httpErrorToMsg $ e

getRespBody :: String -> BearerTokenCredentials -> ErrorT AuthError IO (Response BL.ByteString)
getRespBody url bearerTokenCredentials = do
  let creds = getCredentials bearerTokenCredentials
      authContent = B.concat ["Basic ", creds]
      opts = defaults & header "Authorization" .~ [authContent]
  liftReq $ postWith opts url ["grant_type" := ("client_credentials" :: B.ByteString)]

parseRespBody :: Response BL.ByteString -> Either AuthError B.ByteString
parseRespBody body = do
  tokenV <- maybeToEither InvalidCredentials $ body ^? responseBody . L.key "access_token"
  case (fromJSON tokenV :: Result String) of
    Success v -> return . C.pack $ v
    Error err -> Left $ InvalidJSON err

getBearerToken :: String -> BearerTokenCredentials -> ErrorT AuthError IO BearerToken
getBearerToken url bearerTokenCredentials = do
  r <- getRespBody url bearerTokenCredentials
  token <- ErrorT . return $ parseRespBody r
  return $ BearerToken token
