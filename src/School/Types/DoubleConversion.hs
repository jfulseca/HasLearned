module School.Types.DoubleConversion
( doubleRange
, fromBinary
, getDouble
, putDouble
, toBinary
, fromMatrixDouble
, toMatrixDouble
) where

import Control.Monad (replicateM)
import Data.ByteString (ByteString, unpack)
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.IEEE754 (getFloat64le, putFloat64le)
import Data.Serialize.Put (Put, runPut)
import Numeric.LinearAlgebra ((><), Matrix, R, toLists)
import School.Types.TypeName (TypeName(..))

putDouble :: Double -> Put
putDouble = putFloat64le

getDouble :: Get Double
getDouble = getFloat64le

toBinary :: Double -> ByteString
toBinary = runPut . putDouble

fromBinary :: ByteString -> Either String Double
fromBinary = runGet getDouble

doubleZero :: Double
doubleZero = 0

doubleRange :: (Double, Double)
doubleRange = let
  b = floatRadix doubleZero
  e = floatDigits doubleZero
  (_, e') = floatRange doubleZero
  m = b ^ e - 1
  n = e' - e
  p = encodeFloat m n
  in (-p, p)

toMatrixDouble :: TypeName
               -> Int
               -> Int
               -> (ByteString -> Either String
                                        (Matrix R))
toMatrixDouble DBL64B nRows nCols =
  runGet (getDoubleMatrixDouble nRows nCols)
toMatrixDouble INT08B nRows nCols =
   Right
 . (nRows >< nCols)
 . map (fromIntegral . fromEnum)
 . unpack
toMatrixDouble dType _ _ = const . Left $
  "Conversion to Matrix Double undefined for " ++ (show dType)

getDoubleMatrixDouble :: Int
                      -> Int
                      -> Get (Matrix Double)
getDoubleMatrixDouble nRows nCols = do
  let nElements = nRows * nCols
  list <- replicateM (nElements) getDouble
  return $ (nRows >< nCols) list

fromMatrixDouble :: TypeName
                 -> Matrix Double
                 -> ByteString
fromMatrixDouble DBL64B matrix = runPut $
  mapM_ putDouble (concat . toLists $ matrix)
fromMatrixDouble INT16B _ = undefined
fromMatrixDouble INT08B _ = undefined
