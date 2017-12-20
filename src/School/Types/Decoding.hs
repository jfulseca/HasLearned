module School.Types.Decoding
( binToDouble
, binToInt
, getDouble
, getInt
, binToMatrixDouble
, binToMatrixInt
, runGet
) where

import Control.Monad (replicateM)
import Data.ByteString (ByteString, unpack)
import Data.Int (Int32)
import Data.Serialize.Get (Get, getInt32be, runGet)
import Data.Serialize.IEEE754 (getFloat64be)
import Numeric.LinearAlgebra ((><), Matrix, I, R)
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
  "Decoding to Matrix Double undefined for " ++ (show dType)

getDoubleMatrixDouble :: Int
                      -> Int
                      -> Get (Matrix Double)
getDoubleMatrixDouble nRows nCols = do
  let nElements = nRows * nCols
  list <- replicateM (nElements) getDouble
  return $ (nRows >< nCols) list

getInt :: Get Int32
getInt = getInt32be

binToInt :: ByteString -> Either Error I
binToInt = (fmap fromIntegral)
         . (runGet getInt)

binToMatrixInt :: DataType
               -> Int
               -> Int
               -> (ByteString -> Either Error
                                        (Matrix I))
binToMatrixInt DBL64B _ _ = const . Left $
  "Reject conversion from floating point DBL64B to integral"
binToMatrixInt dType nRows nCols =
  if elem dType [INT32B, INT08B]
    then Right
      . (nRows >< nCols)
      . map (fromIntegral . fromEnum)
      . unpack
    else const . Left $
      "Conversion to Matrix Int undefined for " ++ (show dType)
