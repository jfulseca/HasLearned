{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.FileHeader
( FileHeader(..)
, compatibleHeaders
, headerBuilder
, parseHeader
) where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Data.Attoparsec.ByteString (Parser, anyWord8, parseOnly, take, word8)
import Data.Attoparsec.ByteString.Char8 (char)
import qualified Data.ByteString as B
import Data.ByteString.Conversion (FromByteString(..), ToByteString(..), toByteString')
import Data.Default.Class (Default(..))
import Data.Monoid ((<>))
import Numeric.LinearAlgebra (I)
import Prelude hiding (take)
import School.FileIO.FileType (FileType(..))
import School.Types.DataType (DataType(..), fromIdxIndicator, toIdxIndicator)
import School.Types.Decoding (binToInt)
import School.Types.Encoding (intToBin)
import School.Types.Error (Error)
import School.Types.LiftResult (liftResult)
import School.Utils.Constants (separator)

data FileHeader = FileHeader
  { dataType :: DataType
  , rows :: Int
  , cols :: Int
  } deriving (Eq, Show)

instance Default FileHeader where
  def = FileHeader { dataType = DBL64B
                   , cols = 1
                   , rows = 1
                   }

compatibleHeaders :: FileHeader
                  -> FileHeader
                  -> Bool
compatibleHeaders
  FileHeader { dataType = type1, rows = rows1, cols = cols1 }
  FileHeader { dataType = type2, rows = rows2, cols = cols2 }
    = type1 == type2
   && cols1 == cols2
   && rows2 `mod` rows1 == 0

instance ToByteString FileHeader where
  builder header = sep <> t <> r <> c <> sep where
    sep = builder separator
    t = (builder . dataType) header
    r = (builder 'r') <> (builder . rows) header
    c = (builder 'c') <> (builder . cols) header

intGetter :: Parser I
intGetter = take 4 >>= (\bytes -> liftResult $ binToInt bytes)

parseIdxHeader :: Parser FileHeader
parseIdxHeader = do
  _ <- word8 0
  _ <- word8 0
  t <- fromEnum <$> anyWord8
  dataType <- liftResult $ fromIdxIndicator t
  d <- fromEnum <$> anyWord8
  dims <- (fromIntegral <$>) <$> replicateM d intGetter
  let rows = head dims
  let cols = product . tail $ dims
  return FileHeader { dataType, cols, rows }

parseSmHeader :: Parser FileHeader
parseSmHeader = do
  _ <- char separator
  typeName <- parser
  _ <- char 'r'
  r <- parser
  _ <- char 'c'
  c <- parser
  _ <- char separator
  return $ FileHeader typeName r c

parseHeader :: B.ByteString -> Either Error FileHeader
parseHeader = parseOnly $ parseSmHeader <|> parseIdxHeader

instance FromByteString FileHeader where
  parser = parseSmHeader <|> parseIdxHeader

headerBuilder :: FileType
              -> FileHeader
              -> B.ByteString
headerBuilder SM header = toByteString' header
headerBuilder IDX FileHeader { cols, dataType, rows } =
  let i = toIdxIndicator dataType
  in (B.pack $ toEnum <$> [0, 0, i, 2])
  <> (intToBin . fromIntegral $ rows)
  <> (intToBin . fromIntegral $ cols)
headerBuilder _ _ = undefined
