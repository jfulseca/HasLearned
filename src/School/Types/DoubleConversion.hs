module School.Types.DoubleConversion
( doubleRange
, fromBinary
, getDouble
, putDouble
, toBinary
) where

import Data.ByteString (ByteString)
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.IEEE754 (getFloat64le, putFloat64le)
import Data.Serialize.Put (Put, runPut)

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

