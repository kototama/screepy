module Screepy.Photo
       (Photo(..)) where

import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)

data Photo = Photo { url :: Text
                   , content :: BL.ByteString
                   }
