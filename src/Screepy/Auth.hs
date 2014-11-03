{-# LANGUAGE OverloadedStrings #-}

module Screepy.Auth (createBearerTokenCredentials,
                     getBearerToken
                    ) where

import qualified Network.HTTP.Base as Http
import qualified Codec.Binary.Base64.String as B64
import Network.Wreq
import Network.Wreq.Lens
import Control.Lens
import Data.ByteString.Lazy as L

newtype BearerTokenCredentials =
  BearerTokenCredentials { getCredentials :: String } deriving Show

createBearerTokenCredentials :: String -> String -> BearerTokenCredentials
createBearerTokenCredentials key secret =
  let k = Http.urlEncode key
      s = Http.urlEncode secret
  in
   BearerTokenCredentials $ B64.encode $ k ++ ":" ++ s

getBearerToken :: BearerTokenCredentials -> IO String
getBearerToken bearerTokenCredentials = do
   r <- get "https://google.com"
   case r ^? responseBody of
     Just body -> L.putStr body
     _ -> L.putStr "no body"
   return "win"
