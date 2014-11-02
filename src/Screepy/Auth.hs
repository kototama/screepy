module Screepy.Auth (createBearerTokenCredentials) where

import qualified Network.HTTP.Base as Http
import qualified Codec.Binary.Base64.String as B64

newtype BearerTokenCredentials = BearerTokenCredentials String deriving Show

createBearerTokenCredentials :: String -> String -> BearerTokenCredentials
createBearerTokenCredentials key secret =
  let k = Http.urlEncode key
      s = Http.urlEncode secret
  in
   BearerTokenCredentials $ B64.encode $ k ++ ":" ++ s
