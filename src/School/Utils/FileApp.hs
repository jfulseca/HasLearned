{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}

module School.Utils.FileApp
( checkFile
, fileExists
, getHeaderBytes
, getFileHeaderLength
, getNElements
, putHeader
) where

import Conduit (($$+-), (.|),  ConduitM, await, liftIO, mapC, runConduitRes,
                sinkNull, sourceFileBS, takeCE, takeWhileCE, yield)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.ByteString as B
import Data.Conduit.Binary (sourceFile, sourceFileRange)
import Data.Maybe (isNothing)
import School.FileIO.AppIO (AppIO)
import School.FileIO.ConduitHeader (conduitHeader)
import School.FileIO.FilePath (FilePath)
import School.FileIO.FileType (FileType(..))
import School.FileIO.FileHeader (FileHeader(..), headerBuilder)
import School.Types.DataType (DataType, getSize)
import School.Types.LiftResult (liftResult)
import School.Utils.Constants (binSeparator)
import School.Utils.IO (getFileSize)
import System.Directory (doesFileExist)

liftBytes :: Maybe B.ByteString
          -> AppIO B.ByteString
liftBytes bytes = liftResult $
  maybe (Left "Could not get header bytes")
        Right
        bytes

getNBytes :: FilePath -> Int -> AppIO B.ByteString
getNBytes path n = do
  bytes <- runConduitRes $
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
  bytes <- runConduitRes $
       sourceFileBS path
    .| (takeCE 1 .| sinkNull >> mapC id)
    .| takeWhileCE (/= binSeparator)
    .| await
  liftBytes bytes

putHeader :: FileType
          -> FileHeader
          -> ConduitM B.ByteString B.ByteString AppIO ()
putHeader fType header = do
  yield $ headerBuilder fType header
  mapC id

fileExists :: FilePath -> AppIO ()
fileExists path = do
  exists <- liftIO $ doesFileExist path
  if (not exists)
    then throwError $ "File " ++ path ++ " not found"
    else return ()
  
checkFile :: FilePath
          -> FileType
          -> FileHeader
          -> AppIO ()
checkFile path fType header = do
  let source = sourceFile path :: ConduitM () B.ByteString AppIO ()
  cHeader <- conduitHeader fType header source
  confirmResult <- cHeader $$+- await
  when (isNothing confirmResult) $
    throwError $ "no data in " ++ show path
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
       (throwError $ "Size of " ++ path
              ++ " inconsistent with "
              ++ (show dType))
  liftResult . Right $ quot dataSize elSize
