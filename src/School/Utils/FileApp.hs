{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}

module School.Utils.FileApp
( checkFile
, fileExists
, getHeaderBytes
, getFileHeaderLength
, getNElements
, putHeader
) where

import Conduit ((.|),  await, liftIO, mapC, sinkNull,
                sourceFileBS, takeCE, takeWhileCE, yield)
import Control.Monad (when)
import Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString as B
import Data.Conduit.Binary (sourceFile, sourceFileRange)
import Data.Maybe (isNothing)
import School.FileIO.AppIO (AppIO, liftAppIO, runConduitInAppIO)
import School.FileIO.Confirmer (Confirmer, confirmer)
import School.FileIO.FilePath (FilePath)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..), headerBuilder)
import School.Types.DataType (DataType, getSize)
import School.Utils.Constants (binSeparator)
import School.Utils.IO (getFileSize)
import System.Directory (doesFileExist)

liftBytes :: Maybe B.ByteString
          -> AppIO B.ByteString
liftBytes bytes = liftAppIO $
  maybe (Left "Could not get header bytes")
        Right
        bytes

getNBytes :: FilePath -> Int -> AppIO B.ByteString
getNBytes path n = do
  bytes <- runConduitInAppIO $
       sourceFileRange path Nothing (Just . fromIntegral $ n)
    .| await
  liftBytes bytes

getHeaderBytes :: FileType
               -> FilePath
               -> AppIO B.ByteString
getHeaderBytes CSV _ = undefined
getHeaderBytes IDX path = do
  specBytes <- getNBytes path 4
  let spec = fromEnum <$> B.unpack specBytes
  let dim = spec!!3
  getNBytes path $ (dim + 1) * 4  
getHeaderBytes SM path = do
  bytes <- runConduitInAppIO $
       sourceFileBS path
    .| (takeCE 1 .| sinkNull >> mapC id)
    .| takeWhileCE (/= binSeparator)
    .| await
  liftBytes bytes

putHeader :: FileType
          -> MatrixHeader
          -> Confirmer a
putHeader fType header = do
  yield $ headerBuilder fType header
  mapC id

fileExists :: FilePath -> AppIO ()
fileExists path = do
  exists <- liftIO $ doesFileExist path
  if (not exists)
    then throwE $ "File " ++ path ++ " not found"
    else return ()
  
checkFile :: FilePath
          -> FileType
          -> MatrixHeader
          -> AppIO ()
checkFile path fType header = do
  confirmResult <- runConduitInAppIO $
      sourceFile path
   .| confirmer fType header
   .| await
  when (isNothing confirmResult) $
    throwE $ "no data in " ++ show path
  return ()

getFileHeaderLength :: FileType
                    -> B.ByteString
                    -> Int
getFileHeaderLength SM hBytes =
  B.length hBytes + 2
getFileHeaderLength _ hBytes =
  B.length hBytes

getNElements :: DataType
             -> FilePath
             -> Int
             -> AppIO Integer
getNElements dType path hSize = do
  fullSize <- liftIO $ getFileSize path
  let dataSize = fullSize - fromIntegral hSize
  let elSize = fromIntegral . getSize $ dType
  when (mod dataSize elSize /= 0)
       (throwE $ "Size of " ++ path
              ++ " inconsistent with "
              ++ (show dType))
  liftAppIO . Right $ quot dataSize elSize
