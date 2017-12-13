module School.FileIO.FileType
( FileType(..)
, fromExtension
, toExtension
) where

import Data.Char (toLower, toUpper)
import Data.Default.Class (Default(..))

data FileType = CSV | IDX | SM
  deriving (Eq, Read, Show)

fromExtension :: String -> FileType
fromExtension = read . (map toUpper)

toExtension :: FileType -> String
toExtension = (map toLower) . show

instance Default FileType where
  def = SM
