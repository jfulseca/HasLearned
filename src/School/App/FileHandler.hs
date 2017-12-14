{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}

module School.App.FileHandler
( FHOptions(..) ) where

import Conduit ((.|), ConduitM, mapC)
import Control.Monad (when)
import Control.Monad.Trans.Except (throwE)
import Data.Conduit.Binary (sinkFile, sourceFileRange)
import Data.Default.Class (Default(..))
import Data.Void (Void)
import School.App.AppIO (AppIO, liftAppIO)
import School.App.AppS (AppS)
import School.FileIO.Confirmer (Confirmer)
import School.FileIO.FileApp (FileApp(..))
import School.FileIO.FilePath (FilePath, guessFileType)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..), headerParser)
import School.Types.TypeName (TypeName, getSize)
import School.Utils.FileApp (checkFile, getFileHeaderLength, getHeaderBytes,
                             getNElements, putHeader)

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

getHandler :: FAParams FHOptions -> Confirmer a
getHandler _ = mapC id

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
