module Screepy.Dump (dumpPhotos) where

import Control.Monad.Error
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import qualified Data.ByteString.Lazy         as BL
import Screepy.Photo (Photo(..))
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Data.List.Split              (splitOn)


data DumpError = HttpError String

instance Error DumpError where
  noMsg    = HttpError "HTTP error during dump!"
  strMsg s = HttpError s


mkFilePath :: Photo -> FilePath -> FilePath
mkFilePath photo directory = directory ++ "/" ++ photoName
  where photoName = last $ splitOn "/" (url photo)

sinkPhotos :: FilePath -> Sink Photo (ResourceT IO) ()
sinkPhotos directory = do
    mphoto <- await
    case mphoto of
        Nothing -> return ()
        Just photo -> do
          yield ct =$ CB.sinkFile fp
          sinkPhotos directory
          where fp = mkFilePath photo directory
                ct = BL.toStrict $ content photo

dumpPhotos :: [Photo] -> FilePath -> IO ()
dumpPhotos photos directory =
  runResourceT $ CL.sourceList photos $$ (sinkPhotos directory)
