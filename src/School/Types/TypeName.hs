module School.Types.TypeName
( TypeName(..)
, fromIdxIndicator
, getSize
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString.Conversion (FromByteString(..), ToByteString(..))
import Test.QuickCheck (Arbitrary(..), elements)

data TypeName = INT16B | DBL64B | INT08B
  deriving (Eq, Show)

instance ToByteString TypeName where
  builder = builder . show

parseINT16B :: Parser TypeName
parseINT16B = do
  _ <- char 'I' >> char 'N' >> char 'T'
  _ <- char '1' >> char '6' >> char 'B'
  return INT16B

parseDBL64B :: Parser TypeName
parseDBL64B = do
  _ <- char 'D' >> char 'B' >> char 'L'
  _ <- char '6' >> char '4' >> char 'B'
  return DBL64B

parseINT08B :: Parser TypeName
parseINT08B = do
  _ <- char 'I' >> char 'N' >> char 'T'
  _ <- char '0' >> char '8' >> char 'B'
  return INT08B

parseTypeName :: Parser TypeName
parseTypeName = parseINT16B
            <|> parseDBL64B
            <|> parseINT08B

instance FromByteString TypeName where
  parser = parseTypeName

instance Arbitrary TypeName where
  arbitrary = elements [ INT16B
                       , DBL64B
                       , INT08B
                       ]

fromIdxIndicator :: Int -> Either String TypeName
fromIdxIndicator 8 = Right INT08B
fromIdxIndicator k = Left $
  "Unknown IDX type indicator " ++ (show k)

getSize :: TypeName -> Int
getSize DBL64B = 8
getSize INT16B = 2
getSize INT08B = 1
