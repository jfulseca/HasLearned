module School.FileIO.FileType
( FileType(..)
, fromExtension
, toExtension
) where

import Data.Char (toLower, toUpper)

data FileType = CSV | IDX | SM
  deriving (Read, Show)

fromExtension :: String -> FileType
fromExtension = read . (map toUpper)

toExtension :: FileType -> String
toExtension = (map toLower) . show
