module School.Utils.IO
( getFileSize ) where

import System.FilePath (FilePath)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

getFileSize :: FilePath -> IO Integer
getFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size
