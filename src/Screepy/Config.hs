{-# LANGUAGE OverloadedStrings #-}

module Screepy.Config (loadConfig, Config(..)) where

import Data.ConfigFile
import Data.Either.Utils (forceEither)

data Config = Config { authKey :: String
                     , authSecret :: String
                     , baseUrl :: String
                     , dumpDirectory :: FilePath
                     } deriving Show

loadConfig :: String -> IO Config
loadConfig pathname = do
  val <- readfile emptyCP pathname
  let cp = forceEither val
  let key = forceEither $ get cp "api" "key"
  let secret = forceEither $ get cp "api" "secret"
  let url = forceEither $ get cp "api" "baseurl"
  let dumpDir = forceEither $ get cp "screepy" "dumpDirectory"
  return Config { authKey = key
                , authSecret = secret
                , baseUrl = url
                , dumpDirectory = dumpDir
                }
