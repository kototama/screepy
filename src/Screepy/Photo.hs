module Screepy.Photo
       (Photo(..)) where

import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock      (UTCTime)

data Photo = Photo { url :: String
                   , content :: BL.ByteString
                   , creationTime :: UTCTime
                   }
