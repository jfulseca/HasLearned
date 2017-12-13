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
import System.Exit (die, exitSuccess)

data FileHandlerOptions =
  FileHandlerOptions { end :: Maybe Integer
                     , inHeader :: MatrixHeader
                     , inputFile :: FilePath
                     , inType :: Maybe FileType
                     , offset :: Integer
                     , outputFile :: FilePath
                     , outHeader :: MatrixHeader
                     , outType :: Maybe FileType
                     }

instance Default FileHandlerOptions where
  def = FileHandlerOptions { end = Nothing
                           , inputFile = ""
                           , inType = Just SM
                           , inHeader = defMatrixHeader
                           , offset = 0
                           , outputFile = ""
                           , outType = Just SM
                           , outHeader = defMatrixHeader
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

fileHandler :: FileHandlerOptions -> IO ()
fileHandler inOptions = do
  let options@FileHandlerOptions { end
                                 , inHeader
                                 , inputFile
                                 , inType
                                 , offset
                                 , outHeader
                                 , outputFile
                                 , outType
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
        let addOffset = getHeaderBytes (fromJust inType) inHeader
        let offset' = Just $ addOffset + offset
        let pipeline = sourceFileRange inputFile offset' end
                    .| putHeader (fromJust outType) outHeader
                    .| handler
                    .| sinkFile outputFile
        result <- run $ pipeline
        either showE
               (const exitSuccess)
               result
