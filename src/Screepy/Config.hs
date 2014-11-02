{-# LANGUAGE OverloadedStrings #-}

module Screepy.Config (loadConfig, Config(..)) where

import qualified Data.Yaml.Config as C

data Config = Config { authKey :: String,
                       authSecret :: String
                     } deriving Show

loadConfig :: String -> IO Config 
loadConfig pathname = do
  config <- C.load pathname
  auth <- C.subconfig "auth" config
  authKeyVal <- C.lookup "key" auth
  authSecretVal <- C.lookup "secret" auth
  return Config { authKey = authKeyVal,
                  authSecret = authSecretVal }
  
