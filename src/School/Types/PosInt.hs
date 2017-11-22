{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module School.Types.PosInt
( PosInt
, extractPosInts
, getPosInt
, posInt
) where

import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.ByteString.Conversion (FromByteString(..), ToByteString(..))
import Test.QuickCheck.Modifiers (Positive(..))

type PosInt = Positive Int

getPosInt :: PosInt -> Int
getPosInt (Positive k) = k

posInt :: Int -> Maybe PosInt
posInt k = if k <= 0
  then Nothing
  else Just (Positive k)

parsePosInt :: Parser PosInt
parsePosInt = do
  k <- decimal
  let mPos = posInt k
  case mPos of
    Just pos -> return pos
    Nothing -> fail "Negative number"

instance FromByteString PosInt where
  parser = parsePosInt

instance ToByteString PosInt where
  builder (Positive k) = builder k

instance Num PosInt where
  (Positive n) + (Positive m) = Positive $ n + m
  (Positive n) - (Positive m) = Positive $ n - m
  (Positive n) * (Positive m) = Positive $ n * m
  signum (Positive n) = Positive $ signum n
  negate (Positive n) = Positive $ negate n
  abs (Positive n) = Positive $ abs n
  fromInteger n = Positive $ fromInteger n

instance Real PosInt where
  toRational = toRational . getPosInt

instance Integral PosInt where
  (Positive n) `quotRem` (Positive m) = (pq, pr)
    where (q, r) = n `quotRem` m
          pq = Positive q
          pr = Positive r
  toInteger = toInteger . getPosInt

extractPosInts :: [a -> Int] -> a -> Maybe [PosInt]
extractPosInts getters dat =
  sequence $ posInt <$> (getters <*> pure dat)
