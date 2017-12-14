module School.Types.DataType
( DataType(..)
, fromIdxIndicator
, getSize
, toIdxIndicator
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString.Conversion (FromByteString(..), ToByteString(..))
import Data.Default.Class (Default(..))
import Test.QuickCheck (Arbitrary(..), elements)

data DataType = INT32B | DBL64B | INT08B
  deriving (Eq, Show)

instance ToByteString DataType where
  builder = builder . show

parseINT32B :: Parser DataType
parseINT32B = do
  _ <- char 'I' >> char 'N' >> char 'T'
  _ <- char '3' >> char '2' >> char 'B'
  return INT32B

parseDBL64B :: Parser DataType
parseDBL64B = do
  _ <- char 'D' >> char 'B' >> char 'L'
  _ <- char '6' >> char '4' >> char 'B'
  return DBL64B

parseINT08B :: Parser DataType
parseINT08B = do
  _ <- char 'I' >> char 'N' >> char 'T'
  _ <- char '0' >> char '8' >> char 'B'
  return INT08B

parseDataType :: Parser DataType
parseDataType = parseINT32B
            <|> parseDBL64B
            <|> parseINT08B

instance FromByteString DataType where
  parser = parseDataType

instance Arbitrary DataType where
  arbitrary = elements [ INT32B
                       , DBL64B
                       , INT08B
                       ]

fromIdxIndicator :: Int -> Either String DataType
fromIdxIndicator 8 = Right INT08B
fromIdxIndicator k = Left $
  "Unknown IDX type indicator " ++ (show k)

toIdxIndicator :: DataType -> Int
toIdxIndicator INT08B = 8
toIdxIndicator DBL64B = 13
toIdxIndicator INT32B = 11

getSize :: DataType -> Int
getSize DBL64B = 8
getSize INT32B = 4
getSize INT08B = 1

instance Default DataType where
  def = DBL64B
