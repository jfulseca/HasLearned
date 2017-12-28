{-# LANGUAGE BangPatterns #-}

module School.Types.Decoding
( binToDouble
, binToInt
, getDouble
, getInt
, binToMatrixDouble
, binToListInt
, runGet
) where

import Control.Monad (replicateM)
import Data.ByteString (ByteString, length, splitAt, unpack)
import Data.Int (Int32)
import Data.Serialize.Get (Get, getInt32be, runGet)
import Data.Serialize.IEEE754 (getFloat64be)
import Numeric.LinearAlgebra ((><), Matrix, I, R)
import Prelude hiding (length, splitAt)
import School.Types.DataType (DataType(..))
import School.Types.Error (Error)

getDouble :: Get R
getDouble = getFloat64be

binToDouble :: ByteString -> Either Error R
binToDouble = runGet getDouble

binToMatrixDouble :: DataType
                  -> Int
                  -> Int
                  -> (ByteString -> Either Error
                                           (Matrix R))
binToMatrixDouble DBL64B nRows nCols =
  runGet (getDoubleMatrixDouble nRows nCols)
binToMatrixDouble INT08B nRows nCols =
   Right
 . (nRows >< nCols)
 . map (fromIntegral . fromEnum)
 . unpack
binToMatrixDouble dType _ _ = const . Left $
  "Decoding to Matrix Double undefined for " ++ show dType

getDoubleMatrixDouble :: Int
                      -> Int
                      -> Get (Matrix Double)
getDoubleMatrixDouble nRows nCols = do
  let nElements = nRows * nCols
  list <- replicateM nElements getDouble
  return $ (nRows >< nCols) list

getInt :: Get Int32
getInt = getInt32be

binToInt :: ByteString -> Either Error I
binToInt = fmap fromIntegral . runGet getInt

binToListInt :: DataType
             -> (ByteString -> Either Error [Int])
binToListInt DBL64B = const . Left $
  "Reject conversion from floating point DBL64B to integral"
binToListInt INT32B = loop [] where
  loop acc bytes = let len = length bytes in
    if len < 4
      then return acc
      else do
        let !(next, current) = splitAt (len - 4) bytes
        int <- fromIntegral <$> binToInt current
        loop (int:acc) next
binToListInt INT08B =
    Right
  . map (fromIntegral . fromEnum)
  . unpack
