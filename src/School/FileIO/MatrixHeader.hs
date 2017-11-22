module School.FileIO.MatrixHeader
( MatrixHeader(..)
, compatibleHeaders
, stripSeparators
) where

import Data.Attoparsec.ByteString (Parser, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (FromByteString(..), ToByteString(..))
import Data.Monoid ((<>))
import School.Types.PosInt (PosInt)
import School.Types.TypeName (TypeName)
import School.Utils.Constants (separator)

data MatrixHeader = MatrixHeader
  { dataType :: TypeName
  , rows :: PosInt
  , cols :: PosInt
  } deriving (Eq, Show)

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

stripSeparators :: ByteString ->
                   Either String MatrixHeader
stripSeparators = parseOnly strip where
  strip = do
    _ <- char separator
    header <- parseMatrixHeader
    _ <- char separator
    return header
