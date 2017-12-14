{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}

module School.Utils.FileApp
( checkFile
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

getHeaderBytes :: FileType
               -> FilePath
               -> AppIO B.ByteString
getHeaderBytes CSV _ = undefined
getHeaderBytes IDX path = do
  bytes <- runConduitInAppIO $
       sourceFileRange path Nothing (Just 4)
    .| await
  liftBytes bytes
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

checkFile :: FilePath
          -> FileType
          -> MatrixHeader
          -> AppIO ()
checkFile path fType header = do
  exists <- liftIO $ doesFileExist path
  when (not exists)
       (throwE $ "File " ++ path ++ " not found")
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
