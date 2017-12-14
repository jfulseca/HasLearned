{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.MatrixHeader
( MatrixHeader(..)
, compatibleHeaders
, headerBuilder
, headerParser
, stripSeparators
) where

import Control.Monad (when)
import Data.Attoparsec.ByteString (Parser, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char)
import qualified Data.ByteString as B
import Data.ByteString.Conversion (FromByteString(..), ToByteString(..), fromByteString, toByteString')
import Data.Default.Class (Default(..))
import Data.Monoid ((<>))
import School.FileIO.FileType (FileType(..))
import School.Types.TypeName (TypeName(..), fromIdxIndicator, toIdxIndicator)
import School.Utils.Constants (separator)
import School.Utils.Either (maybeToEither)

data MatrixHeader = MatrixHeader
  { dataType :: TypeName
  , rows :: Int
  , cols :: Int
  } deriving (Eq, Show)

instance Default MatrixHeader where
  def = MatrixHeader { dataType = DBL64B
                     , cols = 1
                     , rows = 1
                     }

compatibleHeaders :: MatrixHeader
                  -> MatrixHeader
                  -> Bool
compatibleHeaders
  MatrixHeader { dataType = type1, rows = rows1, cols = cols1 }
  MatrixHeader { dataType = type2, rows = rows2, cols = cols2 }
    = type1 == type2
   && cols1 == cols2
   && rows2 `mod` rows1 == 0

instance ToByteString MatrixHeader where
  builder header = sep <> t <> r <> c <> sep where
    sep = builder separator
    t = (builder . dataType) header
    r = (builder 'r') <> (builder . rows) header
    c = (builder 'c') <> (builder . cols) header

parseMatrixHeader :: Parser MatrixHeader
parseMatrixHeader = do
  typeName <- parser
  _ <- char 'r'
  r <- parser
  _ <- char 'c'
  c <- parser
  return $ MatrixHeader typeName r c

instance FromByteString MatrixHeader where
  parser = parseMatrixHeader

stripSeparators :: B.ByteString ->
                   Either String MatrixHeader
stripSeparators = parseOnly strip where
  strip = do
    _ <- char separator
    header <- parseMatrixHeader
    _ <- char separator
    return header

headerBuilder :: FileType
              -> MatrixHeader
              -> B.ByteString
headerBuilder SM header = toByteString' header
headerBuilder IDX MatrixHeader { dataType } =
  let i = toIdxIndicator dataType
  in B.pack $ toEnum <$> [0, 0, i, 2]
headerBuilder _ _ = undefined

headerParser :: FileType
             -> Integer
             -> Maybe Int
             -> B.ByteString
             -> Either String MatrixHeader
headerParser IDX nElements nCols bytes = do
  let spec = B.unpack . (B.take 4) $ bytes
  let checks = length spec == 4
            && spec!!0 == 0
            && spec!!1 == 0
  when (not checks)
       (Left "Could not parse IDX header")
  let cols = maybe 1 id nCols
  when (mod nElements (fromIntegral cols) /= 0)
       (Left $ "IDX file size inconsistent with "
            ++ (show cols) ++ " columns")
  let rows = fromIntegral $ quot nElements (fromIntegral cols)
  dataType <- fromIdxIndicator . fromEnum $
    spec!!2
  return MatrixHeader { dataType, cols, rows }
headerParser SM _ nCols bytes = do
  header <- maybeToEither "Could not parse SM header"
                          (fromByteString bytes)
  let columns = maybe (cols header) id nCols
  when (cols header /= columns)
       (Left $ "Byte data inconsistent with "
           ++ (show columns) ++ " columns")
  return header
headerParser _ _ _ _ = undefined
