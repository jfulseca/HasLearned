module School.Types.TypeName
( TypeName(..) ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString.Conversion (FromByteString(..), ToByteString(..))
import Test.QuickCheck (Arbitrary(..), elements)

data TypeName = INT | DBL
  deriving (Eq, Show)

instance ToByteString TypeName where
  builder INT = builder "INT"
  builder DBL = builder "DBL"

parseINT :: Parser TypeName
parseINT = do
  _ <- char 'I' >> char 'N' >> char 'T'
  return INT

parseDBL :: Parser TypeName
parseDBL = do
  _ <- char 'D' >> char 'B' >> char 'L'
  return DBL

parseTypeName :: Parser TypeName
parseTypeName = parseINT <|> parseDBL

instance FromByteString TypeName where
  parser = parseTypeName

instance Arbitrary TypeName where
  arbitrary = elements [INT, DBL]
