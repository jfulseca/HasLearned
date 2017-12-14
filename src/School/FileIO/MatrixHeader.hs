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
import Numeric.LinearAlgebra (I)
import School.FileIO.FileType (FileType(..))
import School.Types.DataType (DataType(..), fromIdxIndicator, toIdxIndicator)
import School.Types.Decoding (binToInt)
import School.Types.Encoding (intToBin)
import School.Utils.Constants (separator)
import School.Utils.Either (maybeToEither)

data MatrixHeader = MatrixHeader
  { dataType :: DataType
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
headerBuilder IDX MatrixHeader { cols, dataType, rows } =
  let i = toIdxIndicator dataType
  in (B.pack $ toEnum <$> [0, 0, i, 2])
  <> (intToBin . fromIntegral $ rows)
  <> (intToBin . fromIntegral $ cols)
headerBuilder _ _ = undefined

getIntN :: Int -> B.ByteString -> Either String I
getIntN n = binToInt . (B.take 4) . (B.drop $ 4 * n)

headerParser :: FileType
             -> B.ByteString
             -> Either String MatrixHeader
headerParser IDX bytes = do
  let spec = B.unpack . (B.take 4) $ bytes
  let checks = length spec == 4
            && spec!!0 == 0
            && spec!!1 == 0
  when (not checks)
       (Left "Could not parse IDX header")
  dataType <- fromIdxIndicator . fromEnum $ spec!!2
  let dim = fromEnum $ spec!!3
  rows <- fromIntegral <$> getIntN 1 bytes
  let go acc i = if i > dim
                   then return acc
                   else do
                     n <- getIntN i bytes
                     go (acc * n) (i + 1)
  cols <- fromIntegral <$> go 1 2
  return MatrixHeader { dataType, cols, rows }
headerParser SM bytes =
 maybeToEither "Could not parse SM header"
               (fromByteString bytes)
headerParser fType _ = Left $
  "Unable to parse " ++ (show fType) ++ " header"
