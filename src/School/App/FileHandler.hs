{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}

module School.App.FileHandler
( FHOptions(..) ) where

import Conduit ((.|), ConduitM, await, liftIO, mapC, sinkNull,
                sourceFileBS, takeCE, takeWhileCE, yield)
import Control.Monad (when)
import Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString as B
import Data.Conduit.Binary (sinkFile, sourceFile, sourceFileRange)
import Data.Default.Class (Default(..))
import Data.Maybe (isNothing)
import Data.Void (Void)
import School.App.AppIO (AppIO, liftAppIO, runConduitInAppIO)
import School.App.AppS (AppS)
import School.FileIO.Confirmer (Confirmer, confirmer)
import School.FileIO.FileApp (FileApp(..))
import School.FileIO.FilePath (FilePath, guessFileType)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..), headerBuilder, headerParser)
import School.Types.TypeName (TypeName, getSize)
import School.Utils.Constants (binSeparator)
import School.Utils.IO (getFileSize)
import System.Directory (doesFileExist)

data FHOptions =
  FHOptions { columnsOpt :: Maybe Int
            , endOpt :: Maybe Integer
            , inFileOpt :: FilePath
            , inFormatOpt :: TypeName
            , inTypeOpt :: Maybe FileType
            , outFileOpt :: FilePath
            , outFormatOpt :: Maybe TypeName
            , outTypeOpt :: Maybe FileType
            , skipRowsOpt :: Integer
            } deriving (Eq, Show)

instance FileApp FHOptions where
  data FAParams FHOptions = FHParams { end :: Maybe Integer
                                     , inputFile :: FilePath
                                     , offset :: Maybe Integer
                                     , outHeader :: MatrixHeader
                                     , outputFile :: FilePath
                                     , outputType :: FileType
                                     } deriving (Eq, Show)
  scan = scanOptions
  prepare = prepareHandler

instance Default FHOptions where
  def = FHOptions { columnsOpt = Nothing
                  , endOpt = Nothing
                  , inFileOpt = ""
                  , inFormatOpt = def
                  , inTypeOpt = Nothing
                  , outFileOpt = ""
                  , outFormatOpt = Nothing
                  , outTypeOpt = Nothing
                  , skipRowsOpt = 0
                  }

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

getHandler :: FAParams FHOptions -> Confirmer a
getHandler _ = mapC id

putHeader :: FileType
          -> MatrixHeader
          -> Confirmer a
putHeader fType header = do
  yield $ headerBuilder fType header
  mapC id

getOffset :: Integer
          -> Maybe Int
          -> TypeName
          -> Integer
          -> Int
          -> AppIO (Maybe Integer)
getOffset 0 _ _ _ hBytes =
  return . Just . fromIntegral $ hBytes
getOffset skipRows columns format nElements hBytes = do
  case columns of
    Nothing -> throwE $ "Must specify number of "
                     ++ "columns when skipping rows"
    Just nCols -> do
      let skipEl = (fromIntegral nCols) * skipRows
      when (skipEl > nElements)
           (throwE $ "Not enough data in file to skip "
                  ++ (show skipRows) ++ " rows")
      let elSize = getSize format
      return . Just $ (skipEl * fromIntegral elSize)
                    + fromIntegral hBytes

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

scanOptions :: FHOptions
            -> AppIO (FAParams FHOptions)
scanOptions FHOptions { columnsOpt
                      , endOpt
                      , inFileOpt
                      , inFormatOpt
                      , inTypeOpt
                      , outFileOpt
                      , outFormatOpt
                      , outTypeOpt
                      , skipRowsOpt
                      } = do
  let (inputType, inputFile) = guessFileType False (inTypeOpt, inFileOpt)
  hBytes <- getHeaderBytes inputType inputFile
  let nHeader = getFileHeaderLength inputType hBytes
  nEl <- getNElements inFormatOpt
                      inputFile
                      nHeader
  offset <- getOffset skipRowsOpt
                      columnsOpt
                      inFormatOpt
                      nEl
                      nHeader
  inHeader <- liftAppIO $
    headerParser inputType nEl columnsOpt hBytes
  checkFile inputFile inputType inHeader
  let (outputType, outputFile) = guessFileType True (outTypeOpt, outFileOpt)
  let outputFormat = maybe inFormatOpt id outFormatOpt
  let outRows = (rows inHeader) - fromIntegral skipRowsOpt
  let outCols = cols inHeader
  let outHeader = MatrixHeader { dataType = outputFormat
                               , cols = outCols
                               , rows = outRows
                               }
  return FHParams { end = endOpt
                  , inputFile
                  , offset
                  , outHeader
                  , outputFile
                  , outputType
                  }

getNElements :: TypeName
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

prepareHandler :: FAParams FHOptions
               -> ConduitM () Void (AppS a) ()
prepareHandler params@FHParams { end
                               , inputFile
                               , offset
                               , outHeader
                               , outputFile
                               , outputType
                               } =
    sourceFileRange inputFile offset end
 .| putHeader outputType outHeader
 .| getHandler params
 .| sinkFile outputFile
