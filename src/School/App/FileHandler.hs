{-# LANGUAGE NamedFieldPuns #-}

module School.App.FileHandler
( FileHandlerOptions(..)
, fileHandler
) where

import Conduit ((.|), await, mapC, yield)
import Control.Monad (when)
import Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString as B
import Data.Conduit.Binary (sinkFile, sourceFile, sourceFileRange)
import Data.Default.Class (Default(..))
import Data.Maybe (fromJust, isNothing)
import School.App.AppIO (AppIO, liftAppIO, runConduitInAppIO)
import School.FileIO.Confirmer (Confirmer, confirmer)
import School.FileIO.FilePath (FilePath, guessFileType)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..), headerBuilder)
import School.Types.TypeName (getSize)

data FileHandlerOptions =
  FileHandlerOptions { end :: Maybe Integer
                     , inHeader :: MatrixHeader
                     , inputFile :: FilePath
                     , inType :: Maybe FileType
                     , outputFile :: FilePath
                     , outType :: Maybe FileType
                     , skipRows :: Maybe Int
                     } deriving (Eq, Show)

instance Default FileHandlerOptions where
  def = FileHandlerOptions { end = Nothing
                           , inputFile = ""
                           , inType = Just SM
                           , inHeader = def
                           , outputFile = ""
                           , outType = Just SM
                           , skipRows = Nothing
                           }

getHeaderBytes :: FileType
               -> MatrixHeader
               -> Integer
getHeaderBytes fType header =
  fromIntegral . B.length $ headerBuilder fType header

getHandler :: FileHandlerOptions -> Confirmer a
getHandler _ = mapC id

putHeader :: FileType
          -> MatrixHeader
          -> Confirmer a
putHeader fType header = do
  yield $ headerBuilder fType header
  mapC id

optionPass :: FileHandlerOptions
           -> Either String FileHandlerOptions
optionPass options@FileHandlerOptions { inputFile
                                      , inHeader
                                      , inType
                                      , outputFile
                                      , outType
                                      , skipRows
                                      } = do
  let MatrixHeader{ rows } = inHeader
  let (inType', inputFile') = guessFileType False (inType, inputFile)
  let enough = if inType' == SM
                 then let skip = maybe 0 id skipRows
                      in rows >= skip
                 else True
  when (not enough) $ Left $ "Not enough data in "
                          ++ (show inputFile')
                          ++ " for skip "
                          ++ (show skipRows)
  let (outType', outputFile') = guessFileType True (outType, outputFile)
  return options { inputFile = inputFile'
                 , inType = Just inType'
                 , outputFile = outputFile'
                 , outType = Just outType'
                 }

getOffset :: Maybe FileType
          -> MatrixHeader
          -> Maybe Int
          -> Maybe Integer
getOffset fType
          header@MatrixHeader { dataType, rows }
          skipRows = let
  offset = maybe 0
                 (\n -> fromIntegral $ n
                                     * rows
                                     * getSize dataType)
                 skipRows
  addOffset = getHeaderBytes (fromJust fType) header
  in Just $ addOffset + offset

getHeader :: MatrixHeader
          -> Maybe Int
          -> MatrixHeader
getHeader header@MatrixHeader{ rows } skipRows =
  let subRows = maybe 0 id skipRows
  in header { rows = rows - subRows }

fileHandler :: FileHandlerOptions -> AppIO ()
fileHandler inOptions = do
  options@FileHandlerOptions { end
                             , inHeader
                             , inputFile
                             , inType
                             , outputFile
                             , outType
                             , skipRows
                             } <- liftAppIO $
    optionPass inOptions
  confirmResult <- runConduitInAppIO $
      sourceFile inputFile
   .| confirmer (fromJust inType) inHeader
   .| await
  when (isNothing confirmResult) $
    throwE $ "no data in " ++ show inputFile
  let handler = getHandler options
  let offset = getOffset inType inHeader skipRows
  let outHeader = getHeader inHeader skipRows
  runConduitInAppIO $
      sourceFileRange inputFile offset end
   .| putHeader (fromJust outType) outHeader
   .| handler
   .| sinkFile outputFile
