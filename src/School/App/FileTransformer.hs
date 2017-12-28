{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}

module School.App.FileTransformer
( FileTransformerOptions(..) ) where

import Conduit ((.|), ConduitM, mapC, mapMC, nullC, takeCE)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Conduit.Binary (sinkFile, sourceFileRange)
import Data.Default.Class (Default(..))
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import School.FileIO.AppIO (AppIO, liftResult)
import School.FileIO.BinConversion (binConversion)
import School.FileIO.FileApp (FileApp(..))
import School.FileIO.FilePath (FilePath, guessFileType)
import School.FileIO.FileType (FileType(..))
import School.FileIO.FileHeader (FileHeader(..), parseHeader)
import School.Types.DataType (DataType, getSize)
import School.Utils.FileApp (checkFile, fileExists, getFileHeaderLength,
                             getHeaderBytes, getNElements, putHeader)

data FileTransformerOptions =
  FileTransformerOptions { inFileOpt :: FilePath
                         , inDataTypeOpt :: DataType
                         , inFileTypeOpt :: Maybe FileType
                         , nRowsOpt :: Maybe Integer
                         , outFileOpt :: FilePath
                         , outDataTypeOpt :: Maybe DataType
                         , outFileTypeOpt :: Maybe FileType
                         , processLineOpt :: Maybe Int
                         , skipRowsOpt :: Maybe Integer
                         } deriving (Eq, Show)

instance FileApp FileTransformerOptions where
  data FAParams FileTransformerOptions =
    FileTransformerParams { inputFile :: FilePath
                          , inputFormat :: DataType
                          , totalOffset :: Maybe Integer
                          , outHeader :: FileHeader
                          , outputFile :: FilePath
                          , outputFormat :: DataType
                          , outputType :: FileType
                          , processChunk :: Int
                          , size :: Maybe Integer
                          } deriving (Eq, Show)
  scan = scanOptions
  prepare = prepareHandler

instance Default FileTransformerOptions where
  def = FileTransformerOptions { inFileOpt = ""
                               , inDataTypeOpt = def
                               , inFileTypeOpt = Nothing
                               , nRowsOpt = Nothing
                               , outFileOpt = ""
                               , outDataTypeOpt = Nothing
                               , outFileTypeOpt = Nothing
                               , processLineOpt = Nothing
                               , skipRowsOpt = Nothing
                               }

getOffset :: Integer
          -> DataType
          -> Integer
          -> Int
          -> Either String Integer
getOffset skipRows dType nElements nCols = do
  let skipEl = fromIntegral nCols * skipRows
  when (skipEl > nElements)
       (Left $ "Not enough data in file to skip "
            ++ show skipRows ++ " rows")
  let elSize = getSize dType
  return $ skipEl * fromIntegral elSize

getExtent :: Integer
          -> DataType
          -> Integer
          -> Int
          -> Integer
          -> Either String Integer
getExtent size dType nElements nCols skip = do
  let sizeEls = fromIntegral nCols * size
  when (sizeEls + skip > nElements)
       (Left $ "Not enough data in file to skip "
            ++ show skip ++ " and keep "
            ++ show size ++ " rows")
  let elSize = getSize dType
  return $ sizeEls * fromIntegral elSize

getLimits :: Maybe Integer
          -> Maybe Integer
          -> Int
          -> DataType
          -> Integer
          -> AppIO (Integer, Maybe Integer)
getLimits Nothing Nothing _ _ _ =
  return (0, Nothing)
getLimits (Just skip) Nothing nCols dType nEl = do
  offset <- liftResult $
     getOffset skip dType nEl nCols
  return (offset, Nothing)
getLimits Nothing (Just size) nCols dType nEl = do
  extent <- liftResult $
     getExtent size dType nEl nCols 0
  return (0, Just extent)
getLimits (Just skip) (Just size) nCols dType nEl = do
  offset <- liftResult $
     getOffset skip dType nEl nCols
  extent <- liftResult $
     getExtent size dType nEl nCols skip
  return (offset, Just extent)

scanOptions :: FileTransformerOptions
            -> AppIO (FAParams FileTransformerOptions)
scanOptions FileTransformerOptions { inFileOpt
                                   , inDataTypeOpt
                                   , inFileTypeOpt
                                   , nRowsOpt
                                   , outFileOpt
                                   , outDataTypeOpt
                                   , outFileTypeOpt
                                   , processLineOpt
                                   , skipRowsOpt
                                   } = do
  let (inputType, inputFile) = guessFileType False (inFileTypeOpt, inFileOpt)
  fileExists inputFile
  hBytes <- getHeaderBytes inputType inputFile
  let nHeader = getFileHeaderLength hBytes
  inHeader@FileHeader { cols } <- liftResult $ parseHeader hBytes
  checkFile inputFile inHeader
  nEl <- getNElements inDataTypeOpt
                      inputFile
                      nHeader
  (offset, size) <- getLimits skipRowsOpt
                              nRowsOpt
                              cols
                              inDataTypeOpt
                              nEl
  let totalOffset = Just $ offset + fromIntegral nHeader
  let (outputType, outputFile) = guessFileType True (outFileTypeOpt, outFileOpt)
  let outputFormat = fromMaybe inDataTypeOpt outDataTypeOpt
  let skip = maybe 0 fromIntegral skipRowsOpt
  let outRows = maybe (rows inHeader - skip)
                      fromIntegral
                      nRowsOpt
  let outHeader = FileHeader { dataType = outputFormat
                               , cols
                               , rows = outRows
                               }
  let nLines = fromMaybe 1 processLineOpt
  let processChunk = getSize inDataTypeOpt * cols * nLines
  return FileTransformerParams { inputFormat = inDataTypeOpt
                               , inputFile
                               , totalOffset
                               , outHeader
                               , outputFile
                               , outputFormat
                               , outputType
                               , processChunk
                               , size
                               }


getHandler :: FAParams FileTransformerOptions
           -> ConduitM ByteString ByteString AppIO ()
getHandler FileTransformerParams { inputFormat
                                 , outputFormat
                                 , processChunk
                                 } =
  if inputFormat == outputFormat
    then mapC id
    else do
      let conversion = liftResult
                     . binConversion inputFormat outputFormat
      let loop = do
            takeCE processChunk .| mapMC conversion
            isEmpty <- nullC
            unless isEmpty loop
      loop

prepareHandler :: FAParams FileTransformerOptions
               -> ConduitM () Void AppIO ()
prepareHandler p@FileTransformerParams { inputFile
                                       , totalOffset
                                       , outHeader
                                       , outputFile
                                       , outputType
                                       , size
                                       } =
    sourceFileRange inputFile totalOffset size
 .| getHandler p
 .| putHeader outputType outHeader
 .| sinkFile outputFile
