module School.FileIO.FilePath
( FilePath
, ensureExtension 
, getFileType
, guessFileType
) where

import Data.Default.Class (def)
import School.FileIO.FileType (FileType, fromExtension, toExtension)
import System.FilePath ((<.>), FilePath, hasExtension, takeExtension)

ensureExtension :: FileType -> FilePath -> FilePath
ensureExtension fType path =
  if hasExtension path
    then path
    else path <.> toExtension fType

getFileType :: FilePath -> FileType
getFileType path =
  if hasExtension path
    then fromExtension . takeExtension $ path
    else def

guessFileType :: Bool
              -> (Maybe FileType, FilePath)
              -> (FileType, FilePath)
guessFileType ensure (fType, path) =
  let t = maybe (getFileType path) id fType
      p = if ensure
            then ensureExtension t path
            else path
  in (t, p)
