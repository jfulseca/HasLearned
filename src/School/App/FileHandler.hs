{-# LANGUAGE NamedFieldPuns #-}

module School.App.FileHandler
( FileHandlerOptions(..)
, defFileHandlerOptions
, fileHandler
) where

import Conduit ((.|), ConduitM, await, mapC, yield)
import qualified Data.ByteString as B
import Data.Conduit.Binary (sinkFile, sourceFile, sourceFileRange)
import Data.Void (Void)
import School.App.AppS (AppS, runAppSConduitDefState)
import School.FileIO.Confirmer (Confirmer, confirmer)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..), defMatrixHeader, headerBuilder)
import System.FilePath (FilePath)
import System.Exit (die, exitSuccess)

data FileHandlerOptions =
  FileHandlerOptions { end :: Maybe Integer
                     , inHeader :: MatrixHeader
                     , inputFile :: FilePath
                     , inType :: FileType
                     , offset :: Integer
                     , outputFile :: FilePath
                     , outHeader :: MatrixHeader
                     , outType :: FileType
                     }

defFileHandlerOptions :: FileHandlerOptions
defFileHandlerOptions =
  FileHandlerOptions { end = Nothing
                     , inputFile = ""
                     , inType = SM
                     , inHeader = defMatrixHeader
                     , offset = 0
                     , outputFile = ""
                     , outType = SM
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

fileHandler :: FileHandlerOptions -> IO ()
fileHandler options@FileHandlerOptions { end
                                     , inHeader
                                     , inputFile
                                     , inType
                                     , offset
                                     , outHeader
                                     , outputFile
                                     , outType
                                     } = do
  confirmResult <- run $
      sourceFile inputFile
   .| confirmer inType inHeader
   .| await
  case confirmResult of
    Left e -> showE e
    Right confirmed -> case confirmed of
      Nothing -> showE $ "no data in " ++ show inputFile
      Just _ -> do
        let handler = getHandler options
        let addOffset = getHeaderBytes inType inHeader
        let offset' = Just $ addOffset + offset
        let pipeline = sourceFileRange inputFile offset' end
                    .| putHeader outType outHeader
                    .| handler
                    .| sinkFile outputFile
        result <- run $ pipeline
        either showE
               (const exitSuccess)
               result
