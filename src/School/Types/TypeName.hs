module School.Types.TypeName
( TypeName(..) ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString.Conversion (FromByteString(..), ToByteString(..))
import Test.QuickCheck (Arbitrary(..), elements)

data TypeName = INT16B | DBL64B
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

parseTypeName :: Parser TypeName
parseTypeName = parseINT16B <|> parseDBL64B

instance FromByteString TypeName where
  parser = parseTypeName

instance Arbitrary TypeName where
  arbitrary = elements [INT16B, DBL64B]
