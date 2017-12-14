{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}

module School.App.FileTransformer
( FileTransformerOptions(..) ) where

import Conduit ((.|), ConduitM, mapC)
import Control.Monad (when)
import Control.Monad.Trans.Except (throwE)
import Data.Conduit.Binary (sinkFile, sourceFileRange)
import Data.Default.Class (Default(..))
import Data.Void (Void)
import School.FileIO.AppIO (AppIO, liftAppIO)
import School.App.AppS (AppS)
import School.FileIO.Confirmer (Confirmer)
import School.FileIO.FileApp (FileApp(..))
import School.FileIO.FilePath (FilePath, guessFileType)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..), headerParser)
import School.Types.DataType (DataType, getSize)
import School.Utils.FileApp (checkFile, getFileHeaderLength, getHeaderBytes,
                             getNElements, putHeader)

data FileTransformerOptions =
  FileTransformerOptions { columnsOpt :: Maybe Int
                         , endOpt :: Maybe Integer
                         , inFileOpt :: FilePath
                         , inDataTypeOpt :: DataType
                         , inFileTypeOpt :: Maybe FileType
                         , outFileOpt :: FilePath
                         , outDataTypeOpt :: Maybe DataType
                         , outFileTypeOpt :: Maybe FileType
                         , skipRowsOpt :: Integer
                         } deriving (Eq, Show)

instance FileApp FileTransformerOptions where
  data FAParams FileTransformerOptions =
    FileTransformerParams { end :: Maybe Integer
                          , inputFile :: FilePath
                          , offset :: Maybe Integer
                          , outHeader :: MatrixHeader
                          , outputFile :: FilePath
                          , outputType :: FileType
                          } deriving (Eq, Show)
  scan = scanOptions
  prepare = prepareHandler

instance Default FileTransformerOptions where
  def = FileTransformerOptions { columnsOpt = Nothing
                               , endOpt = Nothing
                               , inFileOpt = ""
                               , inDataTypeOpt = def
                               , inFileTypeOpt = Nothing
                               , outFileOpt = ""
                               , outDataTypeOpt = Nothing
                               , outFileTypeOpt = Nothing
                               , skipRowsOpt = 0
                               }

getHandler :: FAParams FileTransformerOptions -> Confirmer a
getHandler _ = mapC id

getOffset :: Integer
          -> Maybe Int
          -> DataType
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

scanOptions :: FileTransformerOptions
            -> AppIO (FAParams FileTransformerOptions)
scanOptions FileTransformerOptions { columnsOpt
                                   , endOpt
                                   , inFileOpt
                                   , inDataTypeOpt
                                   , inFileTypeOpt
                                   , outFileOpt
                                   , outDataTypeOpt
                                   , outFileTypeOpt
                                   , skipRowsOpt
                                   } = do
  let (inputType, inputFile) = guessFileType False (inFileTypeOpt, inFileOpt)
  hBytes <- getHeaderBytes inputType inputFile
  let nHeader = getFileHeaderLength inputType hBytes
  nEl <- getNElements inDataTypeOpt
                      inputFile
                      nHeader
  offset <- getOffset skipRowsOpt
                      columnsOpt
                      inDataTypeOpt
                      nEl
                      nHeader
  inHeader <- liftAppIO $
    headerParser inputType nEl columnsOpt hBytes
  checkFile inputFile inputType inHeader
  let (outputType, outputFile) = guessFileType True (outFileTypeOpt, outFileOpt)
  let outputFormat = maybe inDataTypeOpt id outDataTypeOpt
  let outRows = (rows inHeader) - fromIntegral skipRowsOpt
  let outCols = cols inHeader
  let outHeader = MatrixHeader { dataType = outputFormat
                               , cols = outCols
                               , rows = outRows
                               }
  return FileTransformerParams { end = endOpt
                               , inputFile
                               , offset
                               , outHeader
                               , outputFile
                               , outputType
                               }

prepareHandler :: FAParams FileTransformerOptions
               -> ConduitM () Void (AppS a) ()
prepareHandler p@FileTransformerParams { end
                                       , inputFile
                                       , offset
                                       , outHeader
                                       , outputFile
                                       , outputType
                                       } =
    sourceFileRange inputFile offset end
 .| putHeader outputType outHeader
 .| getHandler p
 .| sinkFile outputFile
