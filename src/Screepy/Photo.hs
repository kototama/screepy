module Screepy.Photo
       (Photo(..)) where

import qualified Data.ByteString.Lazy as BL

data Photo = Photo { url :: String
                   , content :: BL.ByteString
                   }
