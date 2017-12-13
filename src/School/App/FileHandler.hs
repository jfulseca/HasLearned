{-# LANGUAGE NamedFieldPuns #-}

module School.App.FileHandler
( FileHandlerOptions(..)
, fileHandler
) where

import Conduit ((.|), ConduitM, await, mapC, yield)
import qualified Data.ByteString as B
import Data.Conduit.Binary (sinkFile, sourceFile, sourceFileRange)
import Data.Default.Class (Default(..))
import Data.Maybe (fromJust)
import Data.Void (Void)
import School.App.AppS (AppS, runAppSConduitDefState)
import School.FileIO.Confirmer (Confirmer, confirmer)
import School.FileIO.FilePath (FilePath, guessFileType)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..), defMatrixHeader, headerBuilder)
import School.Types.PosInt (getPosInt, posInt)
import School.Types.TypeName (getSize)
import System.Exit (die, exitSuccess)

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
                           , inHeader = defMatrixHeader
                           , outputFile = ""
                           , outType = Just SM
                           , skipRows = Nothing
                           }

getHeaderBytes :: FileType
               -> MatrixHeader
               -> Integer
getHeaderBytes fType header =
  fromIntegral . B.length $ headerBuilder fType header

showE :: String -> IO ()
showE e = die $ "ERROR " ++ e

getHandler :: FileHandlerOptions -> Confirmer a
getHandler _ = mapC id

putHeader :: FileType
          -> MatrixHeader
          -> Confirmer a
putHeader fType header = do
  yield $ headerBuilder fType header
  mapC id

run :: ConduitM () Void (AppS Int) b
    -> IO (Either String b)
run = runAppSConduitDefState

optionPass :: FileHandlerOptions
           -> FileHandlerOptions
optionPass options@FileHandlerOptions { inputFile
                                      , inType
                                      , outputFile
                                      , outType
                                      } = let
  (inType', inputFile') = guessFileType False (inType, inputFile)
  (outType', outputFile') = guessFileType True (outType, outputFile)
  in options { inputFile = inputFile'
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
                                     * getPosInt rows
                                     * getSize dataType)
                 skipRows
  addOffset = getHeaderBytes (fromJust fType) header
  in Just $ addOffset + offset

getHeader :: MatrixHeader
          -> Maybe Int
          -> MatrixHeader
getHeader header@MatrixHeader{ rows } skipRows =
  let subRows = maybe 0 id skipRows
      outRows = fromJust . posInt $
        getPosInt rows - subRows
  in header { rows = outRows }

fileHandler :: FileHandlerOptions -> IO ()
fileHandler inOptions = do
  let options@FileHandlerOptions { end
                                 , inHeader
                                 , inputFile
                                 , inType
                                 , outputFile
                                 , outType
                                 , skipRows
                                 } = optionPass inOptions
  confirmResult <- run $
      sourceFile inputFile
   .| confirmer (fromJust inType) inHeader
   .| await
  case confirmResult of
    Left e -> showE e
    Right confirmed -> case confirmed of
      Nothing -> showE $ "no data in " ++ show inputFile
      Just _ -> do
        let handler = getHandler options
        let offset = getOffset inType inHeader skipRows
        let outHeader = getHeader inHeader skipRows
        let pipeline = sourceFileRange inputFile offset end
                    .| putHeader (fromJust outType) outHeader
                    .| handler
                    .| sinkFile outputFile
        result <- run $ pipeline
        either showE
               (const exitSuccess)
               result
